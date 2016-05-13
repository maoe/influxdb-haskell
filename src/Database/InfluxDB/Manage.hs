{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Database.InfluxDB.Manage
  ( manage

  , ShowQuery
  , qid
  , query
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
import qualified Data.Aeson.Types as A
import qualified Data.Attoparsec.Combinator as AC
import qualified Data.Attoparsec.Text as AT
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Types as HT

import Database.InfluxDB.Types as Types
import Database.InfluxDB.Query hiding (query)
import Database.InfluxDB.Format

manage :: QueryParams -> Query -> IO ()
manage params q = do
  manager' <- either HC.newManager return $ params^.manager
  response <- HC.httpLbs request manager'
  let body = HC.responseBody response
  case eitherDecode' body of
    Left message -> do
      throwIO $ IllformedJSON message body
    Right val -> case A.parse (parseResults (params^.precision)) val of
      A.Success vec
        | V.fromList [()] == vec -> return ()
        | otherwise ->
          fail $ "BUG: expected [{}] but got " ++ show val
            ++ "in Databaes.InfluxDB.Manage.manage"
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
      [ ("q", Just $ fromQuery q)
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
  , _query :: !Query
  , _database :: !Database
  , _duration :: !NominalDiffTime
  } deriving Show

instance QueryResults ShowQuery where
  parseResults _ = parseResultsWith $ \columns fields ->
    maybe (fail "parseResults: parse error") return $ do
      Number (toBoundedInteger -> Just _qid) <-
        V.elemIndex "qid" columns >>= V.indexM fields
      String (formatQuery ftext -> _query) <-
        V.elemIndex "query" columns >>= V.indexM fields
      String (formatDatabase ftext -> _database) <-
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
  parseResults _ = parseResultsWith $ \columns fields ->
    ShowSeries <$> parseKey "key" columns fields

makeLenses ''ShowQuery
makeLenses ''ShowSeries

instance HasDatabase ShowQuery where
  database = Database.InfluxDB.Manage.database
