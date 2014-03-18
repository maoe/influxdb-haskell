{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Data.Function (fix)
import Data.Time.Clock.POSIX
import System.IO
import System.Random
import qualified Data.Vector as V

import qualified Network.HTTP.Client as HC

import Database.InfluxDB.Encode
import Database.InfluxDB.Http
import Database.InfluxDB.Types

oneWeekInSeconds :: Int
oneWeekInSeconds = 7*24*60*60

numPoints :: Int
numPoints = 100

batches :: Int
batches = 100

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  HC.withManager managerSettings $ \manager -> do
    dropDatabase config manager (Database "ctx" Nothing)
      `catch` \(_ :: HC.HttpException) -> return ()
    db <- createDatabase config manager "ctx"
    flip fix batches $ \outerLoop !m -> do
      postWithPrecision config manager db SecondsPrecision $
        flip fix numPoints $ \innerLoop !n -> do
          liftIO $ print (m, n)
          !timestamp <- liftIO $ (-)
            <$> getPOSIXTime
            <*> (fromIntegral <$> randomRIO (0, oneWeekInSeconds))
          !value <- liftIO randomIO
          writePoints "ct1" $ Point value timestamp
          when (n > 0) $ innerLoop (n - 1)
      when (m > 0) $ outerLoop (m - 1)

config :: Config
config = Config
  { configCreds = rootCreds
  , configServer = localServer
  }

managerSettings :: HC.ManagerSettings
managerSettings = HC.defaultManagerSettings
  { HC.managerResponseTimeout = Just $ 60*10^6
  }

data Point = Point !Name !POSIXTime

instance ToSeriesData Point where
  toSeriesData (Point value time) = SeriesData
    { seriesDataColumns = V.fromList ["value", "time"]
    , seriesDataPoints =
        [ V.fromList [toValue value, epochInSeconds time]
        ]
    }

epochInSeconds :: POSIXTime -> Value
epochInSeconds = Int . floor

data Name
  = Foo
  | Bar
  | Baz
  | Quu
  | Qux
  deriving (Enum, Bounded)

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
