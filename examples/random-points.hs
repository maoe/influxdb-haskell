{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import Control.Applicative
import Control.Exception as E
import Control.Monad
import Control.Monad.Trans
import Data.Function (fix)
import Data.Time.Clock.POSIX
import System.Environment
import System.IO
import qualified Data.Text as T
import qualified Data.Vector as V

import System.Random.MWC (Variate(..))
import qualified Network.HTTP.Client as HC
import qualified System.Random.MWC as MWC

import Database.InfluxDB

oneWeekInSeconds :: Int
oneWeekInSeconds = 7*24*60*60

main :: IO ()
main = do
  [read -> (numPoints :: Int), read -> (batches :: Int)] <- getArgs
  hSetBuffering stdout NoBuffering
  config <- newConfig
  HC.withManager managerSettings $ \manager -> do
    dropDatabase config manager (Database "ctx" Nothing)
      `E.catch`
        -- Ignore exceptions here
        \(_ :: HC.HttpException) -> return ()
    db <- createDatabase config manager "ctx"
    gen <- MWC.create
    flip fix batches $ \outerLoop !m -> when (m > 0) $ do
      postWithPrecision config manager db SecondsPrecision $ withSeries "ct1" $
        flip fix numPoints $ \innerLoop !n -> when (n > 0) $ do
          !timestamp <- liftIO $ (-)
            <$> getPOSIXTime
            <*> (fromIntegral <$> uniformR (0, oneWeekInSeconds) gen)
          !value <- liftIO $ uniform gen
          writePoints $ Point value timestamp
          innerLoop $ n - 1
      outerLoop $ m - 1

    result <- query config manager db "select count(value) from ct1;"
    case result of
      [] -> putStrLn "Empty series"
      series:_ -> do
        print $ seriesColumns series
        print $ seriesPoints series
    -- Streaming output
    queryChunked config manager db "select * from ct1;" $ \stream0 ->
      flip fix stream0 $ \loop stream -> case stream of
        Done -> return ()
        Yield series next -> do
          case fromSeriesData series of
            Left reason -> hPutStrLn stderr reason
            Right points -> mapM_ print (points :: [Point])
          putStrLn "--"
          stream' <- next
          loop stream'

newConfig :: IO Config
newConfig = do
  pool <- newServerPool localServer [] -- no backup servers
  return Config
    { configCreds = rootCreds
    , configServerPool = pool
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

instance FromSeriesData Point where
  parseSeriesData columns values =
    parseValues columns $ Point
      <$> values .: "value"
      <*> values .: "time"

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

instance FromValue Name where
  parseValue (String name) = case name of
    "foo" -> return Foo
    "bar" -> return Bar
    "baz" -> return Baz
    "quu" -> return Quu
    "qux" -> return Qux
    _ -> fail $ "Incorrect string: " ++ T.unpack name
  parseValue v = typeMismatch "String" v

instance Variate Name where
  uniform = uniformR (minBound, maxBound)
  uniformR (lower, upper) g = do
    name <- uniformR (fromEnum lower, fromEnum upper) g
    return $! toEnum name

instance FromValue POSIXTime where
  parseValue (Int n) = return $ fromIntegral n
  parseValue (Float d) = return $ realToFrac d
  parseValue v = typeMismatch "Int or Float" v
