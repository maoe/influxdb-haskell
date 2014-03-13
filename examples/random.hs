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
import qualified Data.Text as T

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
  ((config, database), [read -> maxTimes]) <- parseOptions =<< getArgs
  HC.withManager HC.defaultManagerSettings $ \manager ->
    flip fix (maxTimes :: Int) $ \loop !n ->
      when (n > 0) $ do
        r <- randomRIO (0, 10000)
        post config manager database $
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

defaultConfig :: (Config, Database)
defaultConfig = (config, db)
  where
    config = Config
      { configCreds = rootCreds
      , configServer = localServer
      }
    db = Database "testdb" Nothing

options :: [OptDescr ((Config, Database) -> (Config, Database))]
options =
  [ Option ['h'] ["host"]
      (ReqArg (\a s -> s & _1.server.host .~ T.pack a) "HOST")
      "Server host"
  , Option ['p'] ["port"]
      (ReqArg (\a s -> s & _1.server.port .~ read a) "PORT")
      "Server port"
  , Option ['u'] ["user"]
      (ReqArg (\a s -> s & _1.credentials.user .~ T.pack a) "USERNAME")
      "User name"
  , Option ['P'] ["password"]
      (ReqArg (\a s -> s & _1.credentials.password .~ T.pack a) "PASSWORD")
      "Password"
  , Option ['d'] ["database"]
      (ReqArg (\a s -> s & _2 .~ Database (T.pack a) Nothing) "DATABASE")
      "Database name"
  ]

parseOptions :: [String] -> IO ((Config, Database), [String])
parseOptions args = case getOpt Permute options args of
  (o, n, [])
    | not (null n) -> return (foldl (flip id) defaultConfig o, n)
  (_, _, errors) -> do
    putStrLn $ concat errors ++ usageInfo header options
    exitFailure
  where
    header = "Usage: ./influx-example-random [OPTION...] NUM_OF_ITER"
