{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Control.Applicative
import Control.Monad
import Data.Function (fix)
import Data.Maybe
import qualified Data.ByteString.Char8 as BS8
import System.Environment
import System.Random

import qualified Data.Vector as V
import qualified Network.HTTP.Client as HC

import Database.InfluxDB.Encode
import Database.InfluxDB.Http
import Database.InfluxDB.Types

main :: IO ()
main = do
  [read -> maxTimes] <- getArgs
  config <- getConfigFromEnv
  HC.withManager HC.defaultManagerSettings $ \manager ->
    flip fix maxTimes $ \loop !n ->
      when (n > 0) $ do
        r <- randomRIO (0, 10000)
        post config manager $ do
          writePoints "random" (r :: RandVal)
        loop $ n - 1

newtype RandVal = RandVal Int deriving (Num, Random, ToValue)

instance ToSeriesData RandVal where
  toSeriesData (RandVal n) = SeriesData
    { seriesDataColumns = V.fromList ["value"]
    , seriesDataPoints = [V.fromList [toValue n]]
    }

getConfigFromEnv :: IO Config
getConfigFromEnv = do
  host <- lookupEnvFor (BS8.unpack . configHost) "INFLUX_HOST"
  port <- lookupEnvFor (show . configPort) "INFLUX_PORT"
  user <- lookupEnvFor (BS8.unpack . configUser) "INFLUX_USER"
  password <- lookupEnvFor (BS8.unpack . configPassword) "INFLUX_PASSWORD"
  database <- lookupEnvFor (BS8.unpack . configDatabase) "INFLUX_DATABASE"
  return defaultConfig
    { configHost = BS8.pack host
    , configPort = read port
    , configUser = BS8.pack user
    , configPassword = BS8.pack password
    , configDatabase = BS8.pack database
    }
  where
    lookupEnvFor :: (Config -> String) -> String -> IO String
    lookupEnvFor field key =
      fromMaybe (field defaultConfig) <$> lookupEnv key
