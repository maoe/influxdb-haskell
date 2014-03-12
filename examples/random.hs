{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Control.Monad
import Data.Function (fix)
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.Random
import qualified Data.ByteString.Char8 as BS8

import Control.Lens
import qualified Data.Vector as V
import qualified Network.HTTP.Client as HC

import Database.InfluxDB.Encode
import Database.InfluxDB.Http
import Database.InfluxDB.Types
import Database.InfluxDB.Lens

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  (settings, [read -> maxTimes]) <- parseOptions =<< getArgs
  HC.withManager HC.defaultManagerSettings $ \manager ->
    flip fix (maxTimes :: Int) $ \loop !n ->
      when (n > 0) $ do
        r <- randomRIO (0, 10000)
        post settings manager $ do
          writePoints "random" (r :: RandVal)
        putStr "."
        loop $ n - 1
  putStrLn ""

newtype RandVal = RandVal Int deriving (Num, Random, ToValue)

instance ToSeriesData RandVal where
  toSeriesData (RandVal n) = SeriesData
    { seriesDataColumns = V.fromList ["value"]
    , seriesDataPoints = [V.fromList [toValue n]]
    }

defaultSettings :: Settings Database
defaultSettings = Settings
  { settingsUser = "root"
  , settingsPassword = "root"
  , settingsEndpoint = Database
      { databaseServer = defaultServer
      , databaseName = "testdb"
      }
  }

options :: [OptDescr (Settings Database -> Settings Database)]
options =
  [ Option ['h'] ["host"]
      (ReqArg (\a s -> s & endpoint.server.host .~ BS8.pack a) "HOST")
      "Server host"
  , Option ['p'] ["port"]
      (ReqArg (\a s -> s & endpoint.server.port .~ read a) "PORT")
      "Server port"
  , Option ['u'] ["user"]
      (ReqArg (\a s -> s & user .~ BS8.pack a) "USERNAME")
      "User name"
  , Option ['P'] ["password"]
      (ReqArg (\a s -> s & password .~ BS8.pack a) "PASSWORD")
      "Password"
  , Option ['d'] ["database"]
      (ReqArg (\a s -> s & endpoint.database .~ BS8.pack a) "DATABASE")
      "Database name"
  ]

parseOptions :: [String] -> IO (Settings Database, [String])
parseOptions args = case getOpt Permute options args of
  (o, n, [])
    | not (null n) -> return (foldl (flip id) defaultSettings o, n)
  (_, _, errors) -> do
    putStrLn $ concat errors ++ usageInfo header options
    exitFailure
  where
    header = "Usage: ./influx-example-random [OPTION...] NUM_OF_ITER"
