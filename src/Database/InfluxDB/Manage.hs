{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Database.InfluxDB.Manage
  ( manage

  , ShowQuery
  , qid
  , queryText
  , Types.database
  , duration

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
import qualified Network.HTTP.Types as HT

import Database.InfluxDB.Types as Types
import Database.InfluxDB.Query hiding (query)
import qualified Database.InfluxDB.Format as F
import qualified Network.HTTP.Client.Compat as HC

-- | Send a database management query to InfluxDB.
manage :: QueryParams -> Query -> IO ()
manage params q = do
  manager' <- either HC.newManager return $ params^.manager
  response <- HC.httpLbs request manager'
  let body = HC.responseBody response
  case eitherDecode' body of
    Left message -> do
      throwIO $ IllformedJSON message body
    Right val -> case A.parse (parseResults (params^.precision)) val of
      A.Success (_ :: V.Vector Void) -> return ()
      A.Error message -> do
        let status = HC.responseStatus response
        when (HT.statusIsServerError status) $
          throwIO $ ServerError message
        when (HT.statusIsClientError status) $
          throwIO $ BadRequest message request
        fail $ "BUG: " ++ message ++ " in Database.InfluxDB.Manage.manage"
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

data ShowQuery = ShowQuery
  { _qid :: !Int
  , _queryText :: !Query
  , _database :: !Database
  , _duration :: !NominalDiffTime
  } deriving Show

instance QueryResults ShowQuery where
  parseResults _ = parseResultsWith $ \_ _ columns fields ->
    maybe (fail "parseResults: parse error") return $ do
      Number (toBoundedInteger -> Just _qid) <-
        V.elemIndex "qid" columns >>= V.indexM fields
      String (F.formatQuery F.text -> _queryText) <-
        V.elemIndex "query" columns >>= V.indexM fields
      String (F.formatDatabase F.text -> _database) <-
        V.elemIndex "database" columns >>= V.indexM fields
      String (parseDuration -> Right _duration) <-
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
  } deriving Show

instance QueryResults ShowSeries where
  parseResults _ = parseResultsWith $ \_ _ columns fields ->
    ShowSeries <$> parseKey "key" columns fields

makeLensesWith (lensRules & generateSignatures .~ False) ''ShowQuery

-- | Query ID
--
-- >>> v <- query (queryParams "_internal") "SHOW QUERIES" :: IO (V.Vector ShowQuery)
-- >>> v ^.. each.qid
-- [149250]
qid :: Lens' ShowQuery Int

-- | Query text
--
-- >>> v <- query (queryParams "_internal") "SHOW QUERIES" :: IO (V.Vector ShowQuery)
-- >>> v ^.. each.queryText
-- ["SHOW QUERIES"]
queryText :: Lens' ShowQuery Query

database :: Lens' ShowQuery Database

-- |
-- >>> v <- query (queryParams "_internal") "SHOW QUERIES" :: IO (V.Vector ShowQuery)
-- >>> v ^.. each.database
-- ["_internal"]
instance HasDatabase ShowQuery where
  database = Database.InfluxDB.Manage.database

-- | Duration of the query
--
-- >>> v <- query (queryParams "_internal") "SHOW QUERIES" :: IO (V.Vector ShowQuery)
-- >>> v ^.. each.duration
-- [0.06062s]
duration :: Lens' ShowQuery NominalDiffTime

makeLensesWith (lensRules & generateSignatures .~ False) ''ShowSeries

-- | Series name
--
-- >>> v <- query (queryParams "_internal") "SHOW SERIES" :: IO (V.Vector ShowSeries)
-- >>> length $ v ^.. each.key
-- 755
key :: Lens' ShowSeries Key
