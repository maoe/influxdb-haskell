{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  , withSeries
  , writePoints

  -- ** Deleting Points
  -- *** One Time Deletes
  , deleteSeries
  -- *** Regularly Scheduled Deletes (not implemented)
  -- , getScheduledDeletes
  -- , addScheduledDelete
  -- , removeScheduledDelete

  -- * Querying Data
  , query
  , Stream(..)
  , queryChunked

  -- * Administration & Security
  -- ** Creating and Dropping Databases
  , listDatabases
  , createDatabase
  , dropDatabase

  -- ** Security
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
  , listInterfaces
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
import Network.URI (escapeURIString, isAllowedInURI)
import Text.Printf (printf)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.DList as DL
import qualified Data.Text as T

import Control.Retry
import Data.Aeson ((.=))
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

timePrecChar :: TimePrecision -> Char
timePrecChar SecondsPrecision = 's'
timePrecChar MillisecondsPrecision = 'm'
timePrecChar MicrosecondsPrecision = 'u'

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
            (printf "&time_precision=%c" . timePrecChar)
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
writeSeries name a = tell . DL.singleton $ Series
  { seriesName = name
  , seriesData = toSeriesData a
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
deleteSeries Config {..} databaseName seriesName =
  void $ httpLbsWithRetry configServerPool makeRequest configHttpManager
  where
    makeRequest = def
      { HC.method = "DELETE"
      , HC.path = escapeString $ printf "/db/%s/series/%s"
          (T.unpack databaseName)
          (T.unpack seriesName)
      , HC.queryString = escapeString $ printf "u=%s&p=%s"
          (T.unpack credsUser)
          (T.unpack credsPassword)
      }
    Credentials {..} = configCreds

-- TODO: Delete API hasn't been implemented in InfluxDB yet
--
-- getScheduledDeletes
--   :: Config
--   -> HC.Manager
--   -> IO [ScheduledDelete]
-- getScheduledDeletes = do
--   error "getScheduledDeletes: not implemented"
--
-- addScheduledDelete
--   :: Config
--   -> HC.Manager
--   -> IO ScheduledDelete
-- addScheduledDelete =
--   error "addScheduledDeletes: not implemented"
--
-- removeScheduledDelete
--   :: Config
--   -> HC.Manager
--   -> ScheduledDeletes
--   -> IO ()
-- removeScheduledDelete =
--   error "removeScheduledDelete: not implemented"

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
query Config {..} databaseName q = do
  response <- httpLbsWithRetry configServerPool request configHttpManager
  case A.decode (HC.responseBody response) of
    Nothing -> fail $ show response
    Just xs -> case mapM fromSeries xs of
      Left reason -> fail reason
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
    Credentials {..} = configCreds

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
      A.Error message -> fail message
    decode (P.Partial k) = demandPayload (decode . k)
    decode (P.Fail _ _ message) = fail message
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
      Left reason -> fail reason
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
listDatabases Config {..} = do
  response <- httpLbsWithRetry configServerPool makeRequest configHttpManager
  case A.decode (HC.responseBody response) of
    Nothing -> fail $ show response
    Just xs -> return xs
  where
    makeRequest = def
      { HC.path = "/db"
      , HC.queryString = escapeString $ printf "u=%s&p=%s"
          (T.unpack credsUser)
          (T.unpack credsPassword)
      }
    Credentials {..} = configCreds

-- | Create a new database. Requires cluster admin privileges.
createDatabase :: Config -> Text -> IO ()
createDatabase Config {..} name =
  void $ httpLbsWithRetry configServerPool makeRequest configHttpManager
  where
    makeRequest = def
      { HC.method = "POST"
      , HC.requestBody = HC.RequestBodyLBS $ AE.encode $ A.object
          [ "name" .= name
          ]
      , HC.path = "/db"
      , HC.queryString = escapeString $ printf "u=%s&p=%s"
          (T.unpack credsUser)
          (T.unpack credsPassword)
      }
    Credentials {..} = configCreds

-- | Drop a database. Requires cluster admin privileges.
dropDatabase
  :: Config
  -> Text -- ^ Database name
  -> IO ()
dropDatabase Config {..} databaseName =
  void $ httpLbsWithRetry configServerPool makeRequest configHttpManager
  where
    makeRequest = def
      { HC.method = "DELETE"
      , HC.path = escapeString $ printf "/db/%s"
          (T.unpack databaseName)
      , HC.queryString = escapeString $ printf "u=%s&p=%s"
          (T.unpack credsUser)
          (T.unpack credsPassword)
      }
    Credentials {..} = configCreds

-- | List cluster administrators.
listClusterAdmins :: Config -> IO [Admin]
listClusterAdmins Config {..} = do
  response <- httpLbsWithRetry configServerPool makeRequest configHttpManager
  case A.decode (HC.responseBody response) of
    Nothing -> fail $ show response
    Just xs -> return xs
  where
    makeRequest = def
      { HC.path = "/cluster_admins"
      , HC.queryString = escapeString $ printf "u=%s&p=%s"
          (T.unpack credsUser)
          (T.unpack credsPassword)
      }
    Credentials {..} = configCreds

authenticateClusterAdmin :: Config -> IO ()
authenticateClusterAdmin Config {..} =
  void $ httpLbsWithRetry configServerPool makeRequest configHttpManager
  where
    makeRequest = def
      { HC.path = "/cluster_admins/authenticate"
      , HC.queryString = escapeString $ printf "u=%s&p=%s"
          (T.unpack credsUser)
          (T.unpack credsPassword)
      }
    Credentials {..} = configCreds

-- | Add a new cluster administrator. Requires cluster admin privilege.
addClusterAdmin
  :: Config
  -> Text -- ^ Admin name
  -> Text -- ^ Password
  -> IO Admin
addClusterAdmin Config {..} name password = do
  void $ httpLbsWithRetry configServerPool makeRequest configHttpManager
  return Admin
    { adminName = name
    }
  where
    makeRequest = def
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
    Credentials {..} = configCreds

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
listDatabaseUsers Config {..} database = do
  response <- httpLbsWithRetry configServerPool makeRequest configHttpManager
  case A.decode (HC.responseBody response) of
    Nothing -> fail $ show response
    Just xs -> return xs
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
addDatabaseUser Config {..} databaseName name password =
  void $ httpLbsWithRetry configServerPool makeRequest configHttpManager
  where
    makeRequest = def
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
    Credentials {..} = configCreds

-- | Delete an user from the database users.
deleteDatabaseUser
  :: Config
  -> Text -- ^ Database name
  -> Text -- ^ User name
  -> IO ()
deleteDatabaseUser config@Config {..} databaseName userName =
  void $ httpLbsWithRetry configServerPool request configHttpManager
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
updateDatabaseUserPassword config@Config {..} databaseName userName password =
  void $ httpLbsWithRetry configServerPool request configHttpManager
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
grantAdminPrivilegeTo config@Config {..} databaseName userName =
  void $ httpLbsWithRetry configServerPool request configHttpManager
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
revokeAdminPrivilegeFrom config@Config {..} databaseName userName =
  void $ httpLbsWithRetry configServerPool request configHttpManager
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
ping Config {..} = do
  response <- httpLbsWithRetry configServerPool makeRequest configHttpManager
  case A.decode (HC.responseBody response) of
    Nothing -> fail $ show response
    Just status -> return status
  where
    makeRequest = def
      { HC.path = "/ping"
      }

-- | Fetch current list of available interfaces
listInterfaces :: Config -> IO [Text]
listInterfaces Config {..} = do
  response <- httpLbsWithRetry configServerPool makeRequest configHttpManager
  case A.decode (HC.responseBody response) of
    Nothing -> fail $ show response
    Just ifaces -> return ifaces
  where
    makeRequest = def
      { HC.path = "/interfaces"
      }

isInSync :: Config -> IO Bool
isInSync Config {..} = do
  response <- httpLbsWithRetry configServerPool makeRequest configHttpManager
  case decodeBool (HC.responseBody response) of
    Nothing -> fail $ show response
    Just status -> return status
  where
    makeRequest = def
      { HC.path = "/sync"
      , HC.queryString = escapeString $ printf "u=%s&p=%s"
          (T.unpack credsUser)
          (T.unpack credsPassword)
      }
    Credentials {..} = configCreds
    decodeBool lbs = do
      val <- PL.maybeResult $ PL.parse AP.value lbs
      AT.parseMaybe A.parseJSON val

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
  retrySettings <- serverRetrySettings <$> readIORef pool
  recovering retrySettings handlers $ do
    server <- activeServer pool
    f $ makeRequest server
  where
    makeRequest Server {..} = request
      { HC.host = escapeText serverHost
      , HC.port = serverPort
      , HC.secure = serverSsl
      }
    handlers =
      [ Handler $ \e -> case e of
          HC.InternalIOException _ -> do
            failover pool
            return True
          _ -> return False
      ]

escapeText :: Text -> BS.ByteString
escapeText = escapeString . T.unpack

escapeString :: String -> BS.ByteString
escapeString = BS8.pack . escapeURIString isAllowedInURI
