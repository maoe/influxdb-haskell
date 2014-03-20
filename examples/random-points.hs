{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Data.Function (fix)
import Data.Time.Clock.POSIX
import System.Environment
import System.IO
import System.Random
import qualified Data.Vector as V

import qualified Network.HTTP.Client as HC

import Database.InfluxDB.Encode
import Database.InfluxDB.Http
import Database.InfluxDB.Types

oneWeekInSeconds :: Int
oneWeekInSeconds = 7*24*60*60

main :: IO ()
main = do
  [read -> (numPoints :: Int), read -> (batches :: Int)] <- getArgs
  hSetBuffering stdout NoBuffering
  HC.withManager managerSettings $ \manager -> do
    dropDatabase config manager (Database "ctx" Nothing)
      `catch`
        \(_ :: HC.HttpException) -> return ()
    db <- createDatabase config manager "ctx"
    flip fix batches $ \outerLoop !m -> when (m > 0) $ do
      postWithPrecision config manager db SecondsPrecision $
        withSeries "ct1" $ do
          flip fix numPoints $ \innerLoop !n -> when (n > 0) $ do
            liftIO $ print (m, n)
            !timestamp <- liftIO $ (-)
              <$> getPOSIXTime
              <*> (fromIntegral <$> randomRIO (0, oneWeekInSeconds))
            !value <- liftIO randomIO
            let !point = Point value timestamp
            liftIO $ putStrLn $ "Writing " ++ show point
            writePoints point
            innerLoop (n - 1)
      outerLoop (m - 1)

config :: Config
config = Config
  { configCreds = rootCreds
  , configServer = localServer
  }

managerSettings :: HC.ManagerSettings
managerSettings = HC.defaultManagerSettings
  { HC.managerResponseTimeout = Just $ 60*(10 :: Int)^(6 :: Int)
  }

data Point = Point !Name !POSIXTime deriving Show

instance ToSeriesData Point where
  toSeriesColumns _ = V.fromList ["value", "time"]
  toSeriesPoints (Point value time) = V.fromList
    [ toValue value
    , epochInSeconds time
    ]

epochInSeconds :: POSIXTime -> Value
epochInSeconds = Int . floor

data Name
  = Foo
  | Bar
  | Baz
  | Quu
  | Qux
  deriving (Enum, Bounded, Show)

instance ToValue Name where
  toValue Foo = String "foo"
  toValue Bar = String "bar"
  toValue Baz = String "baz"
  toValue Quu = String "quu"
  toValue Qux = String "qux"

instance Random Name where
  random = randomR (minBound, maxBound)
  randomR (lower, upper) g = (toEnum a, g')
    where
      (a, g') = randomR (fromEnum lower, fromEnum upper) g
