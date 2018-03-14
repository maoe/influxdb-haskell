{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Control.Applicative
import Control.Exception
import Control.Monad

import Control.Lens
import Data.Aeson
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import Data.Time.Clock
import Data.Void
import qualified Data.Aeson.Types as A
import qualified Data.Attoparsec.Combinator as AC
import qualified Data.Attoparsec.Text as AT
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Types as HT

import Database.InfluxDB.JSON (getField, parseQueryField)
import Database.InfluxDB.Types as Types
import Database.InfluxDB.Query hiding (query)
import qualified Database.InfluxDB.Format as F

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Database.InfluxDB.Query
-- >>> import Database.InfluxDB.Format ((%))

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
    Right val -> case A.parse (parseResults (params^.precision)) val of
      A.Success (_ :: V.Vector Void) -> return ()
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
  { HC.host = TE.encodeUtf8 $ params^.server.host
  , HC.port = fromIntegral $ params^.server.port
  , HC.secure = params^.server.ssl
  , HC.method = "POST"
  , HC.path = "/query"
  }
  where
    Server {..} = params^.server

-- |
-- >>> v <- query (queryParams "_internal") "SHOW QUERIES" :: IO (V.Vector ShowQuery)
data ShowQuery = ShowQuery
  { showQueryQid :: !Int
  , showQueryText :: !Query
  , showQueryDatabase :: !Database
  , showQueryDuration :: !NominalDiffTime
  }

instance QueryResults ShowQuery where
  parseResults _ = parseResultsWith $ \_ _ columns fields ->
    maybe (fail "parseResults: parse error") return $ do
      Number (toBoundedInteger -> Just showQueryQid) <-
        V.elemIndex "qid" columns >>= V.indexM fields
      String (F.formatQuery F.text -> showQueryText) <-
        V.elemIndex "query" columns >>= V.indexM fields
      String (F.formatDatabase F.text -> showQueryDatabase) <-
        V.elemIndex "database" columns >>= V.indexM fields
      String (parseDuration -> Right showQueryDuration) <-
        V.elemIndex "duration" columns >>= V.indexM fields
      return ShowQuery {..}

parseDuration :: Text -> Either String NominalDiffTime
parseDuration = AT.parseOnly $ sum <$!> durations
  where
    durations = some $ (*)
      <$> fmap fromIntegral int
      <*> unit
      where
        int :: AT.Parser Int
        int = AT.decimal
    unit = AC.choice
      [ 10^^(-6 :: Int) <$ AT.char 'u'
      , 1 <$ AT.char 's'
      , 60 <$ AT.char 'm'
      , 3600 <$ AT.char 'h'
      ]

newtype ShowSeries = ShowSeries
  { _key :: Key
  }

instance QueryResults ShowSeries where
  parseResults _ = parseResultsWith $ \_ _ columns fields -> do
    FieldString name <- getField "key" columns fields >>= parseQueryField
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
-- >> v <- query (queryParams "_internal") "SHOW QUERIES" :: IO (V.Vector ShowQuery)
-- >> v ^.. each.qid
-- >[149250]
qid :: Lens' ShowQuery Int

-- | Query text
--
-- >> v <- query (queryParams "_internal") "SHOW QUERIES" :: IO (V.Vector ShowQuery)
-- >> v ^.. each.queryText
-- >["SHOW QUERIES"]
queryText :: Lens' ShowQuery Query

-- |
-- >> v <- query (queryParams "_internal") "SHOW QUERIES" :: IO (V.Vector ShowQuery)
-- >> v ^.. each.database
-- >["_internal"]
instance HasDatabase ShowQuery where
  database = _database

-- | Duration of the query
--
-- >> v <- query (queryParams "_internal") "SHOW QUERIES" :: IO (V.Vector ShowQuery)
-- >> v ^.. each.duration
-- >[0.06062s]
duration :: Lens' ShowQuery NominalDiffTime

makeLensesWith (lensRules & generateSignatures .~ False) ''ShowSeries

-- | Series name
--
-- >> v <- query (queryParams "_internal") "SHOW SERIES" :: IO (V.Vector ShowSeries)
-- >> length $ v ^.. each.key
-- >755
key :: Lens' ShowSeries Key
