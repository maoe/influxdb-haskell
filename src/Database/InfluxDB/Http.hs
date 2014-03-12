{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Database.InfluxDB.Http
  ( Settings(..)
  , Credentials(..), defaultCreds
  , Server(..), defaultServer
  , Database(..)

  -- * Writing Data
  , post
  , Write
  , writePoints
  , writeSeries
  , writeSeriesList

  -- * Administration & Security
  , createDatabase
  , dropDatabase
  ) where

import Control.Applicative (Applicative)
import Control.Monad.Writer
import Data.ByteString (ByteString)
import Text.Printf (printf)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text.Encoding as TE

import Data.Aeson ((.=))
import Data.DList (DList)
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode as AE
import qualified Data.DList as DL
import qualified Network.HTTP.Client as HC

import Database.InfluxDB.Encode
import Database.InfluxDB.Types

data Settings a = Settings
  { settingsCreds :: !Credentials
  , settingsEndpoint :: !a
  } deriving Show

data Credentials = Credentials
  { credsUser :: !ByteString
  , credsPassword :: !ByteString
  } deriving Show

defaultCreds :: Credentials
defaultCreds = Credentials
  { credsUser = "root"
  , credsPassword = "root"
  }

data Server = Server
  { serverHost :: !ByteString
  , serverPort :: !Int
  } deriving Show

defaultServer :: Server
defaultServer = Server
  { serverHost = "localhost"
  , serverPort = 8086
  }

data Database = Database
  { databaseServer :: !Server
  , databaseName :: !ByteString
  } deriving Show

-----------------------------------------------------------
-- Writing Data

post :: Settings Database -> HC.Manager -> Write a -> IO a
post Settings {..} manager write = do
  request <- makeRequest
  void $ HC.httpLbs request manager
  return a
  where
    (a, series) = runWrite write
    makeRequest = do
      request <- HC.parseUrl url
      return request
        { HC.method = "POST"
        , HC.requestBody = HC.RequestBodyLBS $ AE.encode series
        }
    url = printf "http://%s:%s/db/%s/series?u=%s&p=%s"
      (BS8.unpack serverHost)
      (show serverPort)
      (BS8.unpack databaseName)
      (BS8.unpack credsUser)
      (BS8.unpack credsPassword)
    Database {..} = settingsEndpoint
    Server {..} = databaseServer
    Credentials {..} = settingsCreds

newtype Write a = Write (Writer (DList Series) a)
  deriving (Functor, Applicative, Monad, MonadWriter (DList Series))

runWrite :: Write a -> (a, [Series])
runWrite (Write w) = (a, DL.toList series)
  where
    (a, series) = runWriter w

writePoints
  :: ToSeriesData a
  => ByteString
  -- ^ Series name
  -> a
  -- ^ Series data
  -> Write ()
writePoints name a = tell . DL.singleton $ Series
  { seriesName = name
  , seriesData = toSeriesData a
  }

writeSeries
  :: ToSeries a
  => a
  -> Write ()
writeSeries = tell . DL.singleton . toSeries

writeSeriesList
  :: [Series]
  -> Write ()
writeSeriesList = tell . DL.fromList

-----------------------------------------------------------
-- Administration & Security

createDatabase :: Settings Server -> HC.Manager -> ByteString -> IO Database
createDatabase Settings {..} manager name = do
  request <- makeRequest
  void $ HC.httpLbs request manager
  return Database
    { databaseName = name
    , databaseServer = settingsEndpoint
    }
  where
    makeRequest = do
      request <- HC.parseUrl url
      return request
        { HC.method = "POST"
        , HC.requestBody = HC.RequestBodyLBS $ AE.encode $ A.object
            [ "name" .= TE.decodeUtf8 name
            ]
        }
    url = printf "http://%s:%s/db?u=%s&p=%s"
      (BS8.unpack serverHost)
      (show serverPort)
      (BS8.unpack credsUser)
      (BS8.unpack credsPassword)
    Server {..} = settingsEndpoint
    Credentials {..} = settingsCreds

dropDatabase :: Settings Database -> HC.Manager -> IO ()
dropDatabase Settings {..} manager = do
  request <- makeRequest
  void $ HC.httpLbs request manager
  where
    makeRequest = do
      request <- HC.parseUrl url
      return request
        { HC.method = "DELETE"
        }
    url = printf "http://%s:%s/db/%s?u=%s&p=%s"
      (BS8.unpack serverHost)
      (show serverPort)
      (BS8.unpack databaseName)
      (BS8.unpack credsUser)
      (BS8.unpack credsPassword)
    Database {..} = settingsEndpoint
    Server {..} = databaseServer
    Credentials {..} = settingsCreds
