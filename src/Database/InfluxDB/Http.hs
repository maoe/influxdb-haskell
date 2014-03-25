{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Database.InfluxDB.Http
  ( Config(..)
  , Credentials(..), rootCreds
  , Server(..), localServer
  , TimePrecision(..)
  , Database(..)
  , Series(..)
  -- , ScheduledDelete(..)
  , User(..)
  , Admin(..)

  -- * Writing Data

  -- ** Updating Points
  , post, postWithPrecision
  , SeriesT, ValueT
  , writeSeries
  , withSeries
  , writePoints

  -- ** Deleting Points
  -- *** One Time Deletes (not implemented)
  -- , deleteSeries
  -- *** Regularly Scheduled Deletes (not implemented)
  -- , getScheduledDeletes
  -- , addScheduledDelete
  -- , removeScheduledDelete

  -- * Querying Data

  -- * Administration & Security
  -- ** Creating and Dropping Databases
  , listDatabases
  , createDatabase
  , dropDatabase

  -- ** Security
  -- *** Cluster admin
  , listClusterAdmins
  , addClusterAdmin
  , updateClusterAdminPassword
  , deleteClusterAdmin
  -- *** Database user
  , listDatabaseUsers
  , addDatabaseUser
  , updateDatabaseUserPassword
  , deleteDatabaseUser
  , grantAdminPrivilegeTo
  , revokeAdminPrivilegeFrom
  ) where

import Control.Applicative (Applicative)
import Control.Monad.Identity
import Control.Monad.Writer
import Data.DList (DList)
import Data.IORef (IORef)
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

import Control.Exception.Lifted (Handler(..))
import Control.Retry
import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode as AE
import qualified Network.HTTP.Client as HC

import Database.InfluxDB.Encode
import Database.InfluxDB.Types

data Config = Config
  { configCreds :: !Credentials
  , configServerPool :: IORef ServerPool
  }

rootCreds :: Credentials
rootCreds = Credentials
  { credsUser = "root"
  , credsPassword = "root"
  }

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

post
  :: Config
  -> HC.Manager
  -> Database
  -> SeriesT IO a
  -> IO a
post Config {..} manager database write = do
  (a, series) <- runSeriesT write
  request <- makeRequest series
  void $ httpLbsWithRetry configServerPool request manager
  return a
  where
    makeRequest series = do
      request <- HC.parseUrl url
      return request
        { HC.method = "POST"
        , HC.requestBody = HC.RequestBodyLBS $ AE.encode series
        }
    url = printf "http://localhost/db/%s/series?u=%s&p=%s"
      (T.unpack databaseName)
      (T.unpack credsUser)
      (T.unpack credsPassword)
    Database {databaseName} = database
    Credentials {..} = configCreds

postWithPrecision
  :: Config
  -> HC.Manager
  -> Database
  -> TimePrecision
  -> SeriesT IO a
  -> IO a
postWithPrecision Config {..} manager database timePrec write = do
  (a, series) <- runSeriesT write
  request <- makeRequest series
  void $ httpLbsWithRetry configServerPool request manager
  return a
  where
    makeRequest series = do
      request <- HC.parseUrl url
      return request
        { HC.method = "POST"
        , HC.requestBody = HC.RequestBodyLBS $ AE.encode series
        }
    url = printf "http://localhost/db/%s/series?u=%s&p=%s&time_precision=%c"
      (T.unpack databaseName)
      (T.unpack credsUser)
      (T.unpack credsPassword)
      (timePrecChar timePrec)
    Database {databaseName} = database
    Credentials {..} = configCreds

newtype SeriesT m a = SeriesT (WriterT (DList Series) m a)
  deriving
    ( Functor, Applicative, Monad, MonadIO, MonadTrans
    , MonadWriter (DList Series)
    )

newtype ValueT p m a = ValueT (WriterT (DList (Vector Value)) m a)
  deriving
    ( Functor, Applicative, Monad, MonadIO, MonadTrans
    , MonadWriter (DList (Vector Value))
    )

runSeriesT :: Monad m => SeriesT m a -> m (a, [Series])
runSeriesT (SeriesT w) = do
  (a, series) <- runWriterT w
  return (a, DL.toList series)

-- runWrite :: Write a -> (a, [Series])
-- runWrite = runIdentity . runWriteT

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

withSeries
  :: forall m p. (Monad m, ToSeriesData p)
  => Text
  -- ^ Series name
  -> ValueT p m ()
  -> SeriesT m ()
withSeries name (ValueT w) = do
  (_, values) <- lift $ runWriterT w
  tell $ DL.singleton $ Series
    { seriesName = name
    , seriesData = SeriesData
        { seriesDataColumns = toSeriesColumns (Proxy :: Proxy p)
        , seriesDataPoints = values
        }
    }

writePoints
  :: (Monad m, ToSeriesData p)
  => p
  -> ValueT p m ()
writePoints = tell . DL.singleton . toSeriesPoints

-- TODO: Delete API hasn't been implemented in InfluxDB yet
--
-- deleteSeries
--   :: Config
--   -> HC.Manager
--   -> Series
--   -> IO ()
-- deleteSeries Config {..} manager =
--   error "deleteSeries: not implemented"
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
-- Administration & Security

listDatabases :: Config -> HC.Manager -> IO [Database]
listDatabases Config {..} manager = do
  request <- makeRequest
  response <- httpLbsWithRetry configServerPool request manager
  case A.decode (HC.responseBody response) of
    Nothing -> fail $ show response
    Just xs -> return xs
  where
    makeRequest = HC.parseUrl url
    url = printf "http://localhost/db?u=%s&p=%s"
      (T.unpack credsUser)
      (T.unpack credsPassword)
    Credentials {..} = configCreds

createDatabase :: Config -> HC.Manager -> Text -> IO Database
createDatabase Config {..} manager name = do
  request <- makeRequest
  void $ httpLbsWithRetry configServerPool request manager
  return Database
    { databaseName = name
    , databaseReplicationFactor = Nothing
    }
  where
    makeRequest = do
      request <- HC.parseUrl url
      return request
        { HC.method = "POST"
        , HC.requestBody = HC.RequestBodyLBS $ AE.encode $ A.object
            [ "name" .= name
            ]
        }
    url = printf "http://localhost/db?u=%s&p=%s"
      (T.unpack credsUser)
      (T.unpack credsPassword)
    Credentials {..} = configCreds

dropDatabase :: Config -> HC.Manager -> Database -> IO ()
dropDatabase Config {..} manager database = do
  request <- makeRequest
  void $ httpLbsWithRetry configServerPool request manager
  where
    makeRequest = do
      request <- HC.parseUrl url
      return request
        { HC.method = "DELETE"
        }
    url = printf "http://localhost/db/%s?u=%s&p=%s"
      (T.unpack databaseName)
      (T.unpack credsUser)
      (T.unpack credsPassword)
    Database {databaseName} = database
    Credentials {..} = configCreds

listClusterAdmins
  :: Config
  -> HC.Manager
  -> IO [Admin]
listClusterAdmins Config {..} manager = do
  request <- makeRequest
  response <- httpLbsWithRetry configServerPool request manager
  case A.decode (HC.responseBody response) of
    Nothing -> fail $ show response
    Just xs -> return xs
  where
    makeRequest = HC.parseUrl url
    url = printf "http://localhost/cluster_admins?u=%s&p=%s"
      (T.unpack credsUser)
      (T.unpack credsPassword)
    Credentials {..} = configCreds

addClusterAdmin
  :: Config
  -> HC.Manager
  -> Text
  -> IO Admin
addClusterAdmin Config {..} manager name = do
  request <- makeRequest
  void $ httpLbsWithRetry configServerPool request manager
  return Admin
    { adminUsername = name
    }
  where
    makeRequest = do
      request <- HC.parseUrl url
      return request
        { HC.requestBody = HC.RequestBodyLBS $ AE.encode $ A.object
            [ "name" .= name
            ]
        }
    url = printf "http://localhost/cluster_admins?u=%s&p=%s"
      (T.unpack credsUser)
      (T.unpack credsPassword)
    Credentials {..} = configCreds

updateClusterAdminPassword
  :: Config
  -> HC.Manager
  -> Admin
  -> Text
  -> IO ()
updateClusterAdminPassword Config {..} manager admin password = do
  request <- makeRequest
  void $ httpLbsWithRetry configServerPool request manager
  where
    makeRequest = do
      request <- HC.parseUrl url
      return request
        { HC.method = "POST"
        , HC.requestBody = HC.RequestBodyLBS $ AE.encode $ A.object
            [ "password" .= password
            ]
        }
    url = printf "http://localhost/cluster_admins/%s?u=%s&p=%s"
      (T.unpack adminUsername)
      (T.unpack credsUser)
      (T.unpack credsPassword)
    Admin {adminUsername} = admin
    Credentials {..} = configCreds

deleteClusterAdmin
  :: Config
  -> HC.Manager
  -> Admin
  -> IO ()
deleteClusterAdmin Config {..} manager admin = do
  request <- makeRequest
  void $ httpLbsWithRetry configServerPool request manager
  where
    makeRequest = do
      request <- HC.parseUrl url
      return request
        { HC.method = "DELETE"
        }
    url = printf "http://localhost/cluster_admins/%s?u=%s&p=%s"
      (T.unpack adminUsername)
      (T.unpack credsUser)
      (T.unpack credsPassword)
    Admin {adminUsername} = admin
    Credentials {..} = configCreds

listDatabaseUsers
  :: Config
  -> HC.Manager
  -> IO [User]
listDatabaseUsers Config {..} manager = do
  request <- makeRequest
  response <- httpLbsWithRetry configServerPool request manager
  case A.decode (HC.responseBody response) of
    Nothing -> fail $ show response
    Just xs -> return xs
  where
    makeRequest = HC.parseUrl url
    url = printf "http://localhost/db/%s/users?u=%s&p=%s"
      (T.unpack credsUser)
      (T.unpack credsPassword)
    Credentials {..} = configCreds

addDatabaseUser
  :: Config
  -> HC.Manager
  -> Database
  -> Text
  -> IO User
addDatabaseUser Config {..} manager database name = do
  request <- makeRequest
  void $ httpLbsWithRetry configServerPool request manager
  return User
    { userName = name
    }
  where
    makeRequest = do
      request <- HC.parseUrl url
      return request
        { HC.requestBody = HC.RequestBodyLBS $ AE.encode $ A.object
            [ "name" .= name
            ]
        }
    url = printf "http://localhost/db/%s/users?u=%s&p=%s"
      (T.unpack databaseName)
      (T.unpack credsUser)
      (T.unpack credsPassword)
    Database {databaseName} = database
    Credentials {..} = configCreds

deleteDatabaseUser
  :: Config
  -> HC.Manager
  -> Database
  -> User
  -> IO ()
deleteDatabaseUser Config {..} manager database user = do
  request <- makeRequest
  void $ httpLbsWithRetry configServerPool request manager
  where
    makeRequest = do
      request <- HC.parseUrl url
      return request
        { HC.method = "DELETE"
        }
    url = printf "http://localhost/db/%s/users/%s?u=%s&p=%s"
      (T.unpack databaseName)
      (T.unpack userName)
      (T.unpack credsUser)
      (T.unpack credsPassword)
    Database {databaseName} = database
    User {userName} = user
    Credentials {..} = configCreds

updateDatabaseUserPassword
  :: Config
  -> HC.Manager
  -> Database
  -> User
  -> Text
  -> IO ()
updateDatabaseUserPassword Config {..} manager database user password = do
  request <- makeRequest
  void $ HC.httpLbs request manager
  where
    makeRequest = do
      request <- HC.parseUrl url
      return request
        { HC.method = "POST"
        , HC.requestBody = HC.RequestBodyLBS $ AE.encode $ A.object
            [ "password" .= password
            ]
        }
    url = printf "http://localhost/db/%s/users/%s?u=%s&p=%s"
      (T.unpack databaseName)
      (T.unpack userName)
      (T.unpack credsUser)
      (T.unpack credsPassword)
    Database {databaseName} = database
    User {userName} = user
    Credentials {..} = configCreds

grantAdminPrivilegeTo
  :: Config
  -> HC.Manager
  -> Database
  -> User
  -> IO ()
grantAdminPrivilegeTo Config {..} manager database user = do
  request <- makeRequest
  void $ httpLbsWithRetry configServerPool request manager
  where
    makeRequest = do
      request <- HC.parseUrl url
      return request
        { HC.method = "POST"
        , HC.requestBody = HC.RequestBodyLBS $ AE.encode $ A.object
            [ "admin" .= True
            ]
        }
    url = printf "http://localhost/db/%s/users/%s?u=%s&p=%s"
      (T.unpack databaseName)
      (T.unpack userName)
      (T.unpack credsUser)
      (T.unpack credsPassword)
    Database {databaseName} = database
    User {userName} = user
    Credentials {..} = configCreds

revokeAdminPrivilegeFrom
  :: Config
  -> HC.Manager
  -> Database
  -> User
  -> IO ()
revokeAdminPrivilegeFrom Config {..} manager database user = do
  request <- makeRequest
  void $ httpLbsWithRetry configServerPool request manager
  where
    makeRequest = do
      request <- HC.parseUrl url
      return request
        { HC.method = "POST"
        , HC.requestBody = HC.RequestBodyLBS $ AE.encode $ A.object
            [ "admin" .= False
            ]
        }
    url = printf "http://localhost/db/%s/users/%s?u=%s&p=%s"
      (T.unpack databaseName)
      (T.unpack userName)
      (T.unpack credsUser)
      (T.unpack credsPassword)
    Database {databaseName} = database
    User {userName} = user
    Credentials {..} = configCreds

-----------------------------------------------------------

httpLbsWithRetry
  :: IORef ServerPool
  -> HC.Request
  -> HC.Manager
  -> IO (HC.Response BL.ByteString)
httpLbsWithRetry pool request manager =
  recovering defaultRetrySettings handlers $ do
    server <- activeServer pool
    HC.httpLbs (makeRequest server) manager
  where
    makeRequest Server {..} = request
      { HC.host = escapeText serverHost
      , HC.port = serverPort
      , HC.secure = serverSsl
      }
    handlers =
      [ Handler $ \case
          HC.InternalIOException _ -> do
            failover pool
            return True
          _ -> return False
      ]

defaultRetrySettings :: RetrySettings
defaultRetrySettings = RetrySettings
  { numRetries = limitedRetries 5
  , backoff = True
  , baseDelay = 50
  }

escapeText :: Text -> BS.ByteString
escapeText = BS8.pack . escapeURIString isAllowedInURI . T.unpack
