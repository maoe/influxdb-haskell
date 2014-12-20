{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Database.InfluxDB.Http
  ( Config(..)
  , Credentials(..), rootCreds
  , Server(..), localServer
  , TimePrecision(..)

  -- * Writing Data

  -- ** Updating Points
  , post, postWithPrecision
  , SeriesT, PointT
  , writeSeries
  , writeSeriesData
  , withSeries
  , writePoints

  -- ** Deleting Points
  , deleteSeries

  -- * Querying Data
  , query
  , Stream(..)
  , queryChunked

  -- * Administration & Security
  -- ** Creating and Dropping Databases
  , listDatabases
  , createDatabase
  , dropDatabase

  , DatabaseRequest(..)
  , configureDatabase

  -- ** Security
  -- *** Shard spaces
  , ShardSpaceRequest(..)
  , listShardSpaces
  , createShardSpace
  , dropShardSpace

  -- *** Cluster admin
  , listClusterAdmins
  , authenticateClusterAdmin
  , addClusterAdmin
  , updateClusterAdminPassword
  , deleteClusterAdmin
  -- *** Database user
  , listDatabaseUsers
  , authenticateDatabaseUser
  , addDatabaseUser
  , updateDatabaseUserPassword
  , deleteDatabaseUser
  , grantAdminPrivilegeTo
  , revokeAdminPrivilegeFrom

  -- ** Other API
  , ping
  , isInSync
  ) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Writer
import Data.DList (DList)
import Data.IORef
import Data.Proxy
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word32)
import Network.URI (escapeURIString, isAllowedInURI)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.DList as DL
import qualified Data.Text as T
import Text.Printf (printf)

import Control.Retry
import Data.Aeson ((.=))
import Data.Aeson.TH (deriveToJSON)
import Data.Default.Class (Default(def))
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode as AE
import qualified Data.Aeson.Parser as AP
import qualified Data.Aeson.Types as AT
import qualified Data.Attoparsec.ByteString as P
import qualified Data.Attoparsec.ByteString.Lazy as PL
import qualified Network.HTTP.Client as HC

import Database.InfluxDB.Decode
import Database.InfluxDB.Encode
import Database.InfluxDB.Types
import Database.InfluxDB.Types.Internal (stripPrefixOptions)
import Database.InfluxDB.Stream (Stream(..))
import qualified Database.InfluxDB.Stream as S

#if MIN_VERSION_retry(0, 4, 0)
import Control.Monad.Catch (Handler(..))
#else
import Control.Exception.Lifted (Handler(..))
#endif

-- | Configurations for HTTP API client.
data Config = Config
  { configCreds :: !Credentials
  , configServerPool :: !(IORef ServerPool)
  , configHttpManager :: !HC.Manager
  }

-- | Default credentials.
rootCreds :: Credentials
rootCreds = Credentials
  { credsUser = "root"
  , credsPassword = "root"
  }

-- | Default server location.
localServer :: Server
localServer = Server
  { serverHost = "localhost"
  , serverPort = 8086
  , serverSsl = False
  }

data TimePrecision
  = SecondsPrecision
  | MillisecondsPrecision
  | MicrosecondsPrecision

timePrecString :: TimePrecision -> String
timePrecString SecondsPrecision = "s"
timePrecString MillisecondsPrecision = "ms"
timePrecString MicrosecondsPrecision = "u"

-----------------------------------------------------------
-- Writing Data

-- | Post a bunch of writes for (possibly multiple) series into a database.
post
  :: Config
  -> Text
  -> SeriesT IO a
  -> IO a
post config databaseName =
  postGeneric config databaseName Nothing

-- | Post a bunch of writes for (possibly multiple) series into a database like
-- 'post' but with time precision.
postWithPrecision
  :: Config
  -> Text -- ^ Database name
  -> TimePrecision
  -> SeriesT IO a
  -> IO a
postWithPrecision config databaseName timePrec =
  postGeneric config databaseName (Just timePrec)

postGeneric
  :: Config
  -> Text -- ^ Database name
  -> Maybe TimePrecision
  -> SeriesT IO a
  -> IO a
postGeneric Config {..} databaseName timePrec write = do
  (a, series) <- runSeriesT write
  void $ httpLbsWithRetry configServerPool
    (makeRequest series)
    configHttpManager
  return a
  where
    makeRequest series = def
      { HC.method = "POST"
      , HC.requestBody = HC.RequestBodyLBS $ AE.encode series
      , HC.path = escapeString $ printf "/db/%s/series"
          (T.unpack databaseName)
      , HC.queryString = escapeString $ printf "u=%s&p=%s%s"
          (T.unpack credsUser)
          (T.unpack credsPassword)
          (maybe
            ""
            (printf "&time_precision=%s" . timePrecString)
            timePrec :: String)
      }
    Credentials {..} = configCreds

-- | Monad transformer to batch up multiple writes of series to speed up
-- insertions.
newtype SeriesT m a = SeriesT (WriterT (DList Series) m a)
  deriving
    ( Functor, Applicative, Monad, MonadIO, MonadTrans
    , MonadWriter (DList Series)
    )

-- | Monad transformer to batch up multiple writes of points to speed up
-- insertions.
newtype PointT p m a = PointT (WriterT (DList (Vector Value)) m a)
  deriving
    ( Functor, Applicative, Monad, MonadIO, MonadTrans
    , MonadWriter (DList (Vector Value))
    )

runSeriesT :: Monad m => SeriesT m a -> m (a, [Series])
runSeriesT (SeriesT w) = do
  (a, series) <- runWriterT w
  return (a, DL.toList series)

-- | Write a single series data.
writeSeries
  :: (Monad m, ToSeriesData a)
  => Text
  -- ^ Series name
  -> a
  -- ^ Series data
  -> SeriesT m ()
writeSeries name = writeSeriesData name . toSeriesData

-- | Write a single series data.
writeSeriesData
  :: Monad m
  => Text
  -- ^ Series name
  -> SeriesData
  -- ^ Series data
  -> SeriesT m ()
writeSeriesData name a = tell . DL.singleton $ Series
  { seriesName = name
  , seriesData = a
  }

-- | Write a bunch of data for a single series. Columns for the points don't
-- need to be specified because they can be inferred from the type of @a@.
withSeries
  :: forall m a. (Monad m, ToSeriesData a)
  => Text
  -- ^ Series name
  -> PointT a m ()
  -> SeriesT m ()
withSeries name (PointT w) = do
  (_, values) <- lift $ runWriterT w
  tell $ DL.singleton Series
    { seriesName = name
    , seriesData = SeriesData
        { seriesDataColumns = toSeriesColumns (Proxy :: Proxy a)
        , seriesDataPoints = DL.toList values
        }
    }

-- | Write a data into a series.
writePoints
  :: (Monad m, ToSeriesData a)
  => a
  -> PointT a m ()
writePoints = tell . DL.singleton . toSeriesPoints

deleteSeries
  :: Config
  -> Text -- ^ Database name
  -> Text -- ^ Series name
  -> IO ()
deleteSeries config databaseName seriesName = runRequest_ config request
  where
    request = def
      { HC.method = "DELETE"
      , HC.path = escapeString $ printf "/db/%s/series/%s"
          (T.unpack databaseName)
          (T.unpack seriesName)
      , HC.queryString = escapeString $ printf "u=%s&p=%s"
          (T.unpack credsUser)
          (T.unpack credsPassword)
      }
    Credentials {..} = configCreds config

-----------------------------------------------------------
-- Querying Data

-- | Query a specified database.
--
-- The query format is specified in the
-- <http://influxdb.org/docs/query_language/ InfluxDB Query Language>.
query
  :: FromSeries a
  => Config
  -> Text -- ^ Database name
  -> Text -- ^ Query text
  -> IO [a]
query config databaseName q = do
  xs <- runRequest config request
  case mapM fromSeries xs of
    Left reason -> seriesDecodeError reason
    Right ys -> return ys
  where
    request = def
      { HC.path = escapeString $ printf "/db/%s/series"
          (T.unpack databaseName)
      , HC.queryString = escapeString $ printf "u=%s&p=%s&q=%s"
          (T.unpack credsUser)
          (T.unpack credsPassword)
          (T.unpack q)
      }
    Credentials {..} = configCreds config

-- | Construct streaming output
responseStream :: A.FromJSON a => HC.BodyReader -> IO (Stream IO a)
responseStream body = demandPayload $ \payload ->
  if BS.null payload
    then return Done
    else decode $ parseAsJson payload
  where
    demandPayload k = HC.brRead body >>= k
    decode (P.Done leftover value) = case A.fromJSON value of
      A.Success a -> return $ Yield a $ if BS.null leftover
        then responseStream body
        else decode $ parseAsJson leftover
      A.Error message -> jsonDecodeError message
    decode (P.Partial k) = demandPayload (decode . k)
    decode (P.Fail _ _ message) = jsonDecodeError message
    parseAsJson = P.parse A.json

-- | Query a specified database like 'query' but in a streaming fashion.
queryChunked
  :: FromSeries a
  => Config
  -> Text -- ^ Database name
  -> Text -- ^ Query text
  -> (Stream IO a -> IO b)
  -- ^ Action to handle the resulting stream of series
  -> IO b
queryChunked Config {..} databaseName q f =
  withPool configServerPool request $ \request' ->
    HC.withResponse request' configHttpManager $
      responseStream . HC.responseBody >=> S.mapM parse >=> f
  where
    parse series = case fromSeries series of
      Left reason -> seriesDecodeError reason
      Right a -> return a
    request = def
      { HC.path = escapeString $ printf "/db/%s/series"
          (T.unpack databaseName)
      , HC.queryString = escapeString $ printf "u=%s&p=%s&q=%s&chunked=true"
          (T.unpack credsUser)
          (T.unpack credsPassword)
          (T.unpack q)
      }
    Credentials {..} = configCreds

-----------------------------------------------------------
-- Administration & Security

-- | List existing databases.
listDatabases :: Config -> IO [Database]
listDatabases config = runRequest config request
  where
    request = def
      { HC.path = "/db"
      , HC.queryString = escapeString $ printf "u=%s&p=%s"
          (T.unpack credsUser)
          (T.unpack credsPassword)
      }
    Credentials {..} = configCreds config

-- | Create a new database. Requires cluster admin privileges.
createDatabase :: Config -> Text -> IO ()
createDatabase config name = runRequest_ config request
  where
    request = def
      { HC.method = "POST"
      , HC.requestBody = HC.RequestBodyLBS $ AE.encode $ A.object
          [ "name" .= name
          ]
      , HC.path = "/db"
      , HC.queryString = escapeString $ printf "u=%s&p=%s"
          (T.unpack credsUser)
          (T.unpack credsPassword)
      }
    Credentials {..} = configCreds config

-- | Drop a database. Requires cluster admin privileges.
dropDatabase
  :: Config
  -> Text -- ^ Database name
  -> IO ()
dropDatabase config databaseName = runRequest_ config request
  where
    request = def
      { HC.method = "DELETE"
      , HC.path = escapeString $ printf "/db/%s"
          (T.unpack databaseName)
      , HC.queryString = escapeString $ printf "u=%s&p=%s"
          (T.unpack credsUser)
          (T.unpack credsPassword)
      }
    Credentials {..} = configCreds config


data DatabaseRequest = DatabaseRequest
  { databaseRequestSpaces :: [ShardSpaceRequest]
  , databaseRequestContinuousQueries :: [Text]
  } deriving Show

configureDatabase
  :: Config
  -> Text -- ^ Database name
  -> DatabaseRequest
  -> IO ()
configureDatabase config databaseName databaseRequest =
  runRequest_ config request
  where
    request = def
      { HC.method = "POST"
      , HC.requestBody = HC.RequestBodyLBS $ AE.encode databaseRequest
      , HC.path = escapeString $ printf "/cluster/database_configs/%s"
          (T.unpack databaseName)
      , HC.queryString = escapeString $ printf "u=%s&p=%s"
          (T.unpack credsUser)
          (T.unpack credsPassword)
      }
    Credentials {..} = configCreds config

-- | List shard spaces.
listShardSpaces :: Config -> IO [ShardSpace]
listShardSpaces config = runRequest config request
  where
    request = def
      { HC.path = "/cluster/shard_spaces"
      , HC.queryString = escapeString $ printf "u=%s&p=%s"
          (T.unpack credsUser)
          (T.unpack credsPassword)
      }
    Credentials {..} = configCreds config

data ShardSpaceRequest = ShardSpaceRequest
  { shardSpaceRequestName :: Text
  , shardSpaceRequestRegex :: Text
  , shardSpaceRequestRetentionPolicy :: Text
  , shardSpaceRequestShardDuration :: Text
  , shardSpaceRequestReplicationFactor :: Word32
  , shardSpaceRequestSplit :: Word32
  } deriving Show

-- | Create a shard space.
createShardSpace
  :: Config
  -> Text -- ^ Database
  -> ShardSpaceRequest
  -> IO ()
createShardSpace config databaseName shardSpace = runRequest_ config request
  where
    request = def
      { HC.method = "POST"
      , HC.requestBody = HC.RequestBodyLBS $ AE.encode shardSpace
      , HC.path = escapeString $ printf "/cluster/shard_spaces/%s"
          (T.unpack databaseName)
      , HC.queryString = escapeString $ printf "u=%s&p=%s"
          (T.unpack credsUser)
          (T.unpack credsPassword)
      }
    Credentials {..} = configCreds config

dropShardSpace
  :: Config
  -> Text -- ^ Database name
  -> Text -- ^ Shard space name
  -> IO ()
dropShardSpace config databaseName shardSpaceName = runRequest_ config request
  where
    request = def
      { HC.method = "DELETE"
      , HC.path = escapeString $ printf "/cluster/shard_spaces/%s/%s"
          (T.unpack databaseName)
          (T.unpack shardSpaceName)
      , HC.queryString = escapeString $ printf "u=%s&p=%s"
          (T.unpack credsUser)
          (T.unpack credsPassword)
      }
    Credentials {..} = configCreds config

-- | List cluster administrators.
listClusterAdmins :: Config -> IO [Admin]
listClusterAdmins config = runRequest config request
  where
    request = def
      { HC.path = "/cluster_admins"
      , HC.queryString = escapeString $ printf "u=%s&p=%s"
          (T.unpack credsUser)
          (T.unpack credsPassword)
      }
    Credentials {..} = configCreds config

authenticateClusterAdmin :: Config -> IO ()
authenticateClusterAdmin config = runRequest_ config request
  where
    request = def
      { HC.path = "/cluster_admins/authenticate"
      , HC.queryString = escapeString $ printf "u=%s&p=%s"
          (T.unpack credsUser)
          (T.unpack credsPassword)
      }
    Credentials {..} = configCreds config

-- | Add a new cluster administrator. Requires cluster admin privilege.
addClusterAdmin
  :: Config
  -> Text -- ^ Admin name
  -> Text -- ^ Password
  -> IO Admin
addClusterAdmin config name password = do
  runRequest_ config request
  return Admin
    { adminName = name
    }
  where
    request = def
      { HC.method = "POST"
      , HC.requestBody = HC.RequestBodyLBS $ AE.encode $ A.object
          [ "name" .= name
          , "password" .= password
          ]
      , HC.path = "/cluster_admins"
      , HC.queryString = escapeString $ printf "u=%s&p=%s"
          (T.unpack credsUser)
          (T.unpack credsPassword)
      }
    Credentials {..} = configCreds config

-- | Update a cluster administrator's password. Requires cluster admin
-- privilege.
updateClusterAdminPassword
  :: Config
  -> Admin
  -> Text -- ^ New password
  -> IO ()
updateClusterAdminPassword Config {..} admin password =
  void $ httpLbsWithRetry configServerPool makeRequest configHttpManager
  where
    makeRequest = def
      { HC.method = "POST"
      , HC.requestBody = HC.RequestBodyLBS $ AE.encode $ A.object
          [ "password" .= password
          ]
      , HC.path = escapeString $ printf "/cluster_admins/%s"
          (T.unpack adminName)
      , HC.queryString = escapeString $ printf "u=%s&p=%s"
          (T.unpack credsUser)
          (T.unpack credsPassword)
      }
    Admin {adminName} = admin
    Credentials {..} = configCreds

-- | Delete a cluster administrator. Requires cluster admin privilege.
deleteClusterAdmin
  :: Config
  -> Admin
  -> IO ()
deleteClusterAdmin Config {..} admin =
  void $ httpLbsWithRetry configServerPool makeRequest configHttpManager
  where
    makeRequest = def
      { HC.method = "DELETE"
      , HC.path = escapeString $ printf "/cluster_admins/%s"
          (T.unpack adminName)
      , HC.queryString = escapeString $ printf "u=%s&p=%s"
          (T.unpack credsUser)
          (T.unpack credsPassword)
      }
    Admin {adminName} = admin
    Credentials {..} = configCreds

-- | List database users.
listDatabaseUsers
  :: Config
  -> Text
  -> IO [User]
listDatabaseUsers config@Config {..} database = runRequest config makeRequest
  where
    makeRequest = def
      { HC.path = escapeString $ printf "/db/%s/users"
          (T.unpack database)
      , HC.queryString = escapeString $ printf "u=%s&p=%s"
          (T.unpack credsUser)
          (T.unpack credsPassword)
      }
    Credentials {..} = configCreds

authenticateDatabaseUser
  :: Config
  -> Text -- ^ Database name
  -> IO ()
authenticateDatabaseUser Config {..} database =
  void $ httpLbsWithRetry configServerPool makeRequest configHttpManager
  where
    makeRequest = def
      { HC.path = escapeString $ printf "/db/%s/authenticate"
          (T.unpack database)
      , HC.queryString = escapeString $ printf "u=%s&p=%s"
          (T.unpack credsUser)
          (T.unpack credsPassword)
      }
    Credentials {..} = configCreds

-- | Add an user to the database users.
addDatabaseUser
  :: Config
  -> Text -- ^ Database name
  -> Text -- ^ User name
  -> Text -- ^ Password
  -> IO ()
addDatabaseUser config databaseName name password = runRequest_ config request
  where
    request = def
      { HC.method = "POST"
      , HC.requestBody = HC.RequestBodyLBS $ AE.encode $ A.object
          [ "name" .= name
          , "password" .= password
          ]
      , HC.path = escapeString $ printf "/db/%s/users"
          (T.unpack databaseName)
      , HC.queryString = escapeString $ printf "u=%s&p=%s"
          (T.unpack credsUser)
          (T.unpack credsPassword)
      }
    Credentials {..} = configCreds config

-- | Delete an user from the database users.
deleteDatabaseUser
  :: Config
  -> Text -- ^ Database name
  -> Text -- ^ User name
  -> IO ()
deleteDatabaseUser config databaseName userName = runRequest_ config request
  where
    request = (makeRequestFromDatabaseUser config databaseName userName)
      { HC.method = "DELETE"
      }

-- | Update password for the database user.
updateDatabaseUserPassword
  :: Config
  -> Text -- ^ Database name
  -> Text -- ^ User name
  -> Text -- ^ New password
  -> IO ()
updateDatabaseUserPassword config databaseName userName password =
  runRequest_ config request
  where
    request = (makeRequestFromDatabaseUser config databaseName userName)
      { HC.method = "POST"
      , HC.requestBody = HC.RequestBodyLBS $ AE.encode $ A.object
          [ "password" .= password
          ]
      }

-- | Give admin privilege to the user.
grantAdminPrivilegeTo
  :: Config
  -> Text -- ^ Database name
  -> Text -- ^ User name
  -> IO ()
grantAdminPrivilegeTo config databaseName userName = runRequest_ config request
  where
    request = (makeRequestFromDatabaseUser config databaseName userName)
      { HC.method = "POST"
      , HC.requestBody = HC.RequestBodyLBS $ AE.encode $ A.object
          [ "admin" .= True
          ]
      }

-- | Remove admin privilege from the user.
revokeAdminPrivilegeFrom
  :: Config
  -> Text -- ^ Database name
  -> Text -- ^ User name
  -> IO ()
revokeAdminPrivilegeFrom config databaseName userName =
  runRequest_ config request
  where
    request = (makeRequestFromDatabaseUser config databaseName userName)
      { HC.method = "POST"
      , HC.requestBody = HC.RequestBodyLBS $ AE.encode $ A.object
          [ "admin" .= False
          ]
      }

makeRequestFromDatabaseUser
  :: Config
  -> Text -- ^ Database name
  -> Text -- ^ User name
  -> HC.Request
makeRequestFromDatabaseUser Config {..} databaseName userName = def
  { HC.path = escapeString $ printf "/db/%s/users/%s"
      (T.unpack databaseName)
      (T.unpack userName)
  , HC.queryString = escapeString $ printf "u=%s&p=%s"
      (T.unpack credsUser)
      (T.unpack credsPassword)
  }
  where
    Credentials {..} = configCreds

ping :: Config -> IO Ping
ping config = runRequest config request
  where
    request = def
      { HC.path = "/ping"
      }

isInSync :: Config -> IO Bool
isInSync Config {..} = do
  response <- httpLbsWithRetry configServerPool makeRequest configHttpManager
  case eitherDecodeBool (HC.responseBody response) of
    Left reason -> jsonDecodeError reason
    Right status -> return status
  where
    makeRequest = def
      { HC.path = "/sync"
      , HC.queryString = escapeString $ printf "u=%s&p=%s"
          (T.unpack credsUser)
          (T.unpack credsPassword)
      }
    Credentials {..} = configCreds
    eitherDecodeBool lbs = do
      val <- PL.eitherResult $ PL.parse AP.value lbs
      AT.parseEither A.parseJSON val

-----------------------------------------------------------

httpLbsWithRetry
  :: IORef ServerPool
  -> HC.Request
  -> HC.Manager
  -> IO (HC.Response BL.ByteString)
httpLbsWithRetry pool request manager =
  withPool pool request $ \request' ->
    HC.httpLbs request' manager

withPool
  :: IORef ServerPool
  -> HC.Request
  -> (HC.Request -> IO a)
  -> IO a
withPool pool request f = do
  retryPolicy <- serverRetryPolicy <$> readIORef pool
  recovering retryPolicy handlers $ do
    server <- activeServer pool
    f $ makeRequest server
  where
    makeRequest Server {..} = request
      { HC.host = escapeText serverHost
      , HC.port = serverPort
      , HC.secure = serverSsl
      }
    handlers =
      [
#if MIN_VERSION_retry(0, 5, 0)
        const $
#endif
        Handler $ \e -> case e of
          HC.FailedConnectionException {} -> retry
          HC.FailedConnectionException2 {} -> retry
          HC.InternalIOException {} -> retry
          HC.ResponseTimeout {} -> retry
          _ -> return False
      ]
    retry = True <$ failover pool

escapeText :: Text -> BS.ByteString
escapeText = escapeString . T.unpack

escapeString :: String -> BS.ByteString
escapeString = BS8.pack . escapeURIString isAllowedInURI

decodeJsonResponse
  :: A.FromJSON a
  => HC.Response BL.ByteString
  -> IO a
decodeJsonResponse response =
  case A.eitherDecode (HC.responseBody response) of
    Left reason -> jsonDecodeError reason
    Right a -> return a

runRequest :: A.FromJSON a => Config -> HC.Request -> IO a
runRequest Config {..} req = do
  response <- httpLbsWithRetry configServerPool req configHttpManager
  decodeJsonResponse response

runRequest_ :: Config -> HC.Request -> IO ()
runRequest_ Config {..} req =
  void $ httpLbsWithRetry configServerPool req configHttpManager

-----------------------------------------------------------
-- Aeson instances

deriveToJSON (stripPrefixOptions "shardSpaceRequest") ''ShardSpaceRequest
deriveToJSON (stripPrefixOptions "databaseRequest") ''DatabaseRequest
