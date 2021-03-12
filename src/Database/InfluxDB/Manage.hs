{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-missing-signatures #-}
#else
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
#endif
module Database.InfluxDB.Manage
  ( -- * Management query interface
    Query
  , manage

  -- * Query parameters
  , QueryParams
  , queryParams
  , server
  , database
  , precision
  , manager

  -- * Management query results
  -- ** SHOW QUERIES
  , ShowQuery
  , qid
  , queryText
  , duration

  -- ** SHOW SERIES
  , ShowSeries
  , key
  ) where
import Control.Exception
import Control.Monad

import Control.Lens
import Data.Aeson
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import Data.Time.Clock
import qualified Data.Aeson.Types as A
import qualified Data.Attoparsec.Combinator as AC
import qualified Data.Attoparsec.Text as AT
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Types as HT

import Database.InfluxDB.JSON (getField)
import Database.InfluxDB.Types as Types
import Database.InfluxDB.Query hiding (query)
import qualified Database.InfluxDB.Format as F

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Database.InfluxDB.Query
-- >>> import Database.InfluxDB.Format ((%))
-- >>> import Database.InfluxDB.Manage

-- | Send a database management query to InfluxDB.
--
-- >>> let db = "manage-test"
-- >>> let p = queryParams db
-- >>> manage p $ F.formatQuery ("CREATE DATABASE "%F.database) db
manage :: QueryParams -> Query -> IO ()
manage params q = do
  manager' <- either HC.newManager return $ params^.manager
  response <- HC.httpLbs request manager' `catch` (throwIO . HTTPException)
  let body = HC.responseBody response
  case eitherDecode' body of
    Left message ->
      throwIO $ UnexpectedResponse message request body
    Right val -> do
      let parser = parseQueryResultsWith
            (params ^. decoder)
            (params ^. precision)
      case A.parse parser val of
        A.Success (_ :: V.Vector Empty) -> return ()
        A.Error message -> do
          let status = HC.responseStatus response
          when (HT.statusIsServerError status) $
            throwIO $ ServerError message
          when (HT.statusIsClientError status) $
            throwIO $ ClientError message request
          throwIO $ UnexpectedResponse
            ("BUG: " ++ message ++ " in Database.InfluxDB.Manage.manage")
            request
            (encode val)
  where
    request = HC.setQueryString qs $ manageRequest params
    qs =
      [ ("q", Just $ F.fromQuery q)
      ]

manageRequest :: QueryParams -> HC.Request
manageRequest params = HC.defaultRequest
  { HC.host = TE.encodeUtf8 _host
  , HC.port = fromIntegral _port
  , HC.secure = _ssl
  , HC.method = "POST"
  , HC.path = "/query"
  }
  where
    Server {..} = params^.server

-- |
-- >>> v <- query @ShowQuery (queryParams "_internal") "SHOW QUERIES"
data ShowQuery = ShowQuery
  { showQueryQid :: !Int
  , showQueryText :: !Query
  , showQueryDatabase :: !Database
  , showQueryDuration :: !NominalDiffTime
  }

instance QueryResults ShowQuery where
  parseMeasurement _ _ _ columns fields =
    maybe (fail "parseResults: parse error") return $ do
      Number (toBoundedInteger -> Just showQueryQid) <-
        getField "qid" columns fields
      String (F.formatQuery F.text -> showQueryText) <-
        getField "query" columns fields
      String (F.formatDatabase F.text -> showQueryDatabase) <-
        getField "database" columns fields
      String (parseDuration -> Right showQueryDuration) <-
        getField "duration" columns fields
      return ShowQuery {..}

parseDuration :: Text -> Either String NominalDiffTime
parseDuration = AT.parseOnly duration
  where
    duration = (*)
      <$> fmap (fromIntegral @Int) AT.decimal
      <*> unit
    unit = AC.choice
      [ 10^^(-6 :: Int) <$ AT.string "µs"
      , 1 <$ AT.char 's'
      , 60 <$ AT.char 'm'
      , 3600 <$ AT.char 'h'
      ]

newtype ShowSeries = ShowSeries
  { _key :: Key
  }

instance QueryResults ShowSeries where
  parseMeasurement _ _ _ columns fields = do
    name <- getField "key" columns fields >>= parseJSON
    return $ ShowSeries $ F.formatKey F.text name

makeLensesWith
  ( lensRules
    & generateSignatures .~ False
    & lensField .~ lookingupNamer
      [ ("showQueryQid", "qid")
      , ("showQueryText", "queryText")
      , ("showQueryDatabase", "_database")
      , ("showQueryDuration", "duration")
      ]
  ) ''ShowQuery

-- | Query ID
--
-- >>> v <- query @ShowQuery (queryParams "_internal") "SHOW QUERIES"
-- >>> v ^.. each.qid
-- ...
qid :: Lens' ShowQuery Int

-- | Query text
--
-- >>> v <- query @ShowQuery (queryParams "_internal") "SHOW QUERIES"
-- >>> v ^.. each.queryText
-- ...
queryText :: Lens' ShowQuery Query

-- |
-- >>> v <- query @ShowQuery (queryParams "_internal") "SHOW QUERIES"
-- >>> v ^.. each.database
-- ...
instance HasDatabase ShowQuery where
  database = _database

-- | Duration of the query
--
-- >>> v <- query @ShowQuery (queryParams "_internal") "SHOW QUERIES"
-- >>> v ^.. each.duration
-- ...
duration :: Lens' ShowQuery NominalDiffTime

makeLensesWith (lensRules & generateSignatures .~ False) ''ShowSeries

-- | Series name
--
-- >>> v <- query @ShowSeries (queryParams "_internal") "SHOW SERIES"
-- >>> length $ v ^.. each.key
-- ...
key :: Lens' ShowSeries Key
