{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Database.InfluxDB.Http
  ( Config
  , configHost
  , configPort
  , configUser
  , configPassword
  , configDatabase
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

data Config = Config
  { configHost :: !ByteString
  , configPort :: !Int
  , configUser :: !ByteString
  , configPassword :: !ByteString
  , configDatabase :: !ByteString
  }

configToUrl :: Config -> String
configToUrl Config {..} = printf "http://%s:%s/db/%s/series?u=%s&p=%s"
  (BS8.unpack configHost)
  (show configPort)
  (BS8.unpack configDatabase)
  (BS8.unpack configUser)
  (BS8.unpack configPassword)

post :: Config -> HC.Manager -> Write a -> IO a
post config manager write = do
  request <- makeRequest
  HC.httpLbs request manager
  return a
  where
    (a, series) = runWrite write
    makeRequest = do
      request <- HC.parseUrl $ configToUrl config
      return request
        { HC.method = "POST"
        , HC.requestBody = HC.RequestBodyLBS $ AE.encode series
        }

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

