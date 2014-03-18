{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
  , Write
  , writePoints
  , writeSeries
  , writeSeriesList

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
import Data.Text (Text)
import Text.Printf (printf)
import qualified Data.Text as T

import Data.Aeson ((.=))
import Data.DList (DList)
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode as AE
import qualified Data.DList as DL
import qualified Network.HTTP.Client as HC

import Database.InfluxDB.Encode
import Database.InfluxDB.Types

data Config = Config
  { configCreds :: !Credentials
  , configServer :: !Server
  } deriving Show

rootCreds :: Credentials
rootCreds = Credentials
  { credsUser = "root"
  , credsPassword = "root"
  }

localServer :: Server
localServer = Server
  { serverHost = "localhost"
  , serverPort = 8086
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
  -> WriteT IO a
  -> IO a
post Config {..} manager database write = do
  (a, series) <- runWriteT write
  request <- makeRequest series
  void $ HC.httpLbs request manager
  return a
  where
    makeRequest series = do
      request <- HC.parseUrl url
      return request
        { HC.method = "POST"
        , HC.requestBody = HC.RequestBodyLBS $ AE.encode series
        }
    url = printf "http://%s:%s/db/%s/series?u=%s&p=%s"
      (T.unpack serverHost)
      (show serverPort)
      (T.unpack databaseName)
      (T.unpack credsUser)
      (T.unpack credsPassword)
    Database {databaseName} = database
    Server {..} = configServer
    Credentials {..} = configCreds

postWithPrecision
  :: Config
  -> HC.Manager
  -> Database
  -> TimePrecision
  -> WriteT IO a
  -> IO a
postWithPrecision Config {..} manager database timePrec write = do
  (a, series) <- runWriteT write
  request <- makeRequest series
  void $ HC.httpLbs request manager
  return a
  where
    makeRequest series = do
      request <- HC.parseUrl url
      return request
        { HC.method = "POST"
        , HC.requestBody = HC.RequestBodyLBS $ AE.encode series
        }
    url = printf "http://%s:%s/db/%s/series?u=%s&p=%s&time_precision=%c"
      (T.unpack serverHost)
      (show serverPort)
      (T.unpack databaseName)
      (T.unpack credsUser)
      (T.unpack credsPassword)
      (timePrecChar timePrec)
    Database {databaseName} = database
    Server {..} = configServer
    Credentials {..} = configCreds

type Write = WriteT Identity

newtype WriteT m a = WriteT (WriterT (DList Series) m a)
  deriving
    ( Functor, Applicative, Monad, MonadIO, MonadTrans
    , MonadWriter (DList Series)
    )

runWriteT :: Monad m => WriteT m a -> m (a, [Series])
runWriteT (WriteT w) = do
  (a, series) <- runWriterT w
  return (a, DL.toList series)

-- runWrite :: Write a -> (a, [Series])
-- runWrite = runIdentity . runWriteT

writePoints
  :: (Monad m, ToSeriesData a)
  => Text
  -- ^ Series name
  -> a
  -- ^ Series data
  -> WriteT m ()
writePoints name a = tell . DL.singleton $ Series
  { seriesName = name
  , seriesData = toSeriesData a
  }

writeSeries
  :: (Monad m, ToSeries a)
  => a
  -> WriteT m ()
writeSeries = tell . DL.singleton . toSeries

writeSeriesList
  :: Monad m
  => [Series]
  -> WriteT m ()
writeSeriesList = tell . DL.fromList

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
  response <- HC.httpLbs request manager
  case A.decode (HC.responseBody response) of
    Nothing -> fail $ show response
    Just xs -> return xs
  where
    makeRequest = HC.parseUrl url
    url = printf "http://%s:%s/db?u=%s&p=%s"
      (T.unpack serverHost)
      (show serverPort)
      (T.unpack credsUser)
      (T.unpack credsPassword)
    Server {..} = configServer
    Credentials {..} = configCreds

createDatabase :: Config -> HC.Manager -> Text -> IO Database
createDatabase Config {..} manager name = do
  request <- makeRequest
  void $ HC.httpLbs request manager
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
    url = printf "http://%s:%s/db?u=%s&p=%s"
      (T.unpack serverHost)
      (show serverPort)
      (T.unpack credsUser)
      (T.unpack credsPassword)
    Server {..} = configServer
    Credentials {..} = configCreds

dropDatabase :: Config -> HC.Manager -> Database -> IO ()
dropDatabase Config {..} manager database = do
  request <- makeRequest
  void $ HC.httpLbs request manager
  where
    makeRequest = do
      request <- HC.parseUrl url
      return request
        { HC.method = "DELETE"
        }
    url = printf "http://%s:%s/db/%s?u=%s&p=%s"
      (T.unpack serverHost)
      (show serverPort)
      (T.unpack databaseName)
      (T.unpack credsUser)
      (T.unpack credsPassword)
    Database {databaseName} = database
    Server {..} = configServer
    Credentials {..} = configCreds

listClusterAdmins
  :: Config
  -> HC.Manager
  -> IO [Admin]
listClusterAdmins Config {..} manager = do
  request <- makeRequest
  response <- HC.httpLbs request manager
  case A.decode (HC.responseBody response) of
    Nothing -> fail $ show response
    Just xs -> return xs
  where
    makeRequest = HC.parseUrl url
    url = printf "http://%s:%s/cluster_admins?u=%s&p=%s"
      (T.unpack serverHost)
      (show serverPort)
      (T.unpack credsUser)
      (T.unpack credsPassword)
    Server {..} = configServer
    Credentials {..} = configCreds

addClusterAdmin
  :: Config
  -> HC.Manager
  -> Text
  -> IO Admin
addClusterAdmin Config {..} manager name = do
  request <- makeRequest
  void $ HC.httpLbs request manager
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
    url = printf "http://%s:%s/cluster_admins?u=%s&p=%s"
      (T.unpack serverHost)
      (show serverPort)
      (T.unpack credsUser)
      (T.unpack credsPassword)
    Credentials {..} = configCreds
    Server {..} = configServer

updateClusterAdminPassword
  :: Config
  -> HC.Manager
  -> Admin
  -> Text
  -> IO ()
updateClusterAdminPassword Config {..} manager admin password = do
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
    url = printf "http://%s:%s/cluster_admins/%s?u=%s&p=%s"
      (T.unpack serverHost)
      (show serverPort)
      (T.unpack adminUsername)
      (T.unpack credsUser)
      (T.unpack credsPassword)
    Admin {adminUsername} = admin
    Credentials {..} = configCreds
    Server {..} = configServer

deleteClusterAdmin
  :: Config
  -> HC.Manager
  -> Admin
  -> IO ()
deleteClusterAdmin Config {..} manager admin = do
  request <- makeRequest
  void $ HC.httpLbs request manager
  where
    makeRequest = do
      request <- HC.parseUrl url
      return request
        { HC.method = "DELETE"
        }
    url = printf "http://%s:%s/cluster_admins/%s?u=%s&p=%s"
      (T.unpack serverHost)
      (show serverPort)
      (T.unpack adminUsername)
      (T.unpack credsUser)
      (T.unpack credsPassword)
    Admin {adminUsername} = admin
    Credentials {..} = configCreds
    Server {..} = configServer

listDatabaseUsers
  :: Config
  -> HC.Manager
  -> IO [User]
listDatabaseUsers Config {..} manager = do
  request <- makeRequest
  response <- HC.httpLbs request manager
  case A.decode (HC.responseBody response) of
    Nothing -> fail $ show response
    Just xs -> return xs
  where
    makeRequest = HC.parseUrl url
    url = printf "http://%s:%s/db/%s/users?u=%s&p=%s"
      (T.unpack serverHost)
      (show serverPort)
      (T.unpack credsUser)
      (T.unpack credsPassword)
    Server {..} = configServer
    Credentials {..} = configCreds

addDatabaseUser
  :: Config
  -> HC.Manager
  -> Database
  -> Text
  -> IO User
addDatabaseUser Config {..} manager database name = do
  request <- makeRequest
  void $ HC.httpLbs request manager
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
    url = printf "http://%s:%s/db/%s/users?u=%s&p=%s"
      (T.unpack serverHost)
      (show serverPort)
      (T.unpack databaseName)
      (T.unpack credsUser)
      (T.unpack credsPassword)
    Database {databaseName} = database
    Credentials {..} = configCreds
    Server {..} = configServer

deleteDatabaseUser
  :: Config
  -> HC.Manager
  -> Database
  -> User
  -> IO ()
deleteDatabaseUser Config {..} manager database user = do
  request <- makeRequest
  void $ HC.httpLbs request manager
  where
    makeRequest = do
      request <- HC.parseUrl url
      return request
        { HC.method = "DELETE"
        }
    url = printf "http://%s:%s/db/%s/users/%s?u=%s&p=%s"
      (T.unpack serverHost)
      (show serverPort)
      (T.unpack databaseName)
      (T.unpack userName)
      (T.unpack credsUser)
      (T.unpack credsPassword)
    Database {databaseName} = database
    User {userName} = user
    Credentials {..} = configCreds
    Server {..} = configServer

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
    url = printf "http://%s:%s/db/%s/users/%s?u=%s&p=%s"
      (T.unpack serverHost)
      (show serverPort)
      (T.unpack databaseName)
      (T.unpack userName)
      (T.unpack credsUser)
      (T.unpack credsPassword)
    Database {databaseName} = database
    User {userName} = user
    Credentials {..} = configCreds
    Server {..} = configServer

grantAdminPrivilegeTo
  :: Config
  -> HC.Manager
  -> Database
  -> User
  -> IO ()
grantAdminPrivilegeTo Config {..} manager database user = do
  request <- makeRequest
  void $ HC.httpLbs request manager
  where
    makeRequest = do
      request <- HC.parseUrl url
      return request
        { HC.method = "POST"
        , HC.requestBody = HC.RequestBodyLBS $ AE.encode $ A.object
            [ "admin" .= True
            ]
        }
    url = printf "http://%s:%s/db/%s/users/%s?u=%s&p=%s"
      (T.unpack serverHost)
      (show serverPort)
      (T.unpack databaseName)
      (T.unpack userName)
      (T.unpack credsUser)
      (T.unpack credsPassword)
    Database {databaseName} = database
    User {userName} = user
    Credentials {..} = configCreds
    Server {..} = configServer

revokeAdminPrivilegeFrom
  :: Config
  -> HC.Manager
  -> Database
  -> User
  -> IO ()
revokeAdminPrivilegeFrom Config {..} manager database user = do
  request <- makeRequest
  void $ HC.httpLbs request manager
  where
    makeRequest = do
      request <- HC.parseUrl url
      return request
        { HC.method = "POST"
        , HC.requestBody = HC.RequestBodyLBS $ AE.encode $ A.object
            [ "admin" .= False
            ]
        }
    url = printf "http://%s:%s/db/%s/users/%s?u=%s&p=%s"
      (T.unpack serverHost)
      (show serverPort)
      (T.unpack databaseName)
      (T.unpack userName)
      (T.unpack credsUser)
      (T.unpack credsPassword)
    Database {databaseName} = database
    User {userName} = user
    Credentials {..} = configCreds
    Server {..} = configServer
