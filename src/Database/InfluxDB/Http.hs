{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Database.InfluxDB.Http
  ( Settings(..)
  , Server(..), defaultServer
  , Database(..)

  -- * Writing Data
  , post
  , Write
  , writePoints
  , writeSeries
  , writeSeriesList
  ) where

import Control.Applicative (Applicative)
import Control.Monad.Writer
import Data.ByteString (ByteString)
import Text.Printf (printf)
import qualified Data.ByteString.Char8 as BS8

import Data.DList (DList)
import qualified Data.Aeson.Encode as AE
import qualified Data.DList as DL
import qualified Network.HTTP.Client as HC

import Database.InfluxDB.Encode
import Database.InfluxDB.Types

data Settings a = Settings
  { settingsUser :: ByteString
  , settingsPassword :: ByteString
  , settingsEndpoint :: a
  } deriving Show

data Server = Server
  { serverHost :: ByteString
  , serverPort :: Int
  } deriving Show

defaultServer :: Server
defaultServer = Server
  { serverHost = "localhost"
  , serverPort = 8086
  }

data Database = Database
  { databaseServer :: Server
  , databaseName :: ByteString
  } deriving Show

-----------------------------------------------------------
-- Writing Data

post :: Settings Database -> HC.Manager -> Write a -> IO a
post Settings {..} manager write = do
  request <- makeRequest
  HC.httpLbs request manager
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
      (BS8.unpack settingsUser)
      (BS8.unpack settingsPassword)
    Database {..} = settingsEndpoint
    Server {..} = databaseServer

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

