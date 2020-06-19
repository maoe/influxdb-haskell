{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
import Data.Foldable
import Data.Traversable
import System.Environment
import System.IO
import Text.Printf (printf)

import Control.Lens
import Data.Aeson
import Data.Optional (Optional(Default))
import Data.Time.Clock.POSIX
import System.Random.MWC (Variate(..))
import qualified Control.Foldl as L
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Network.HTTP.Client as HC
import qualified System.Random.MWC as MWC

import Database.InfluxDB
import qualified Database.InfluxDB.Format as F
import qualified Database.InfluxDB.Manage as M

oneWeekInSeconds :: Int
oneWeekInSeconds = 7*24*60*60

main :: IO ()
main = do
  [read -> (numPoints :: Int), read -> (batches :: Int)] <- getArgs
  hSetBuffering stdout NoBuffering
  manager' <- HC.newManager managerSettings

  let
    ctx = "ctx"
    ct1 = "ct1"
    qparams = queryParams ctx
      & manager .~ Right manager'
      & precision .~ RFC3339

  M.manage qparams $ F.formatQuery ("DROP DATABASE "%F.database) ctx
  M.manage qparams $ F.formatQuery ("CREATE DATABASE "%F.database) ctx

  let wparams = writeParams ctx & manager .~ Right manager'

  gen <- MWC.create
  for_ [1..batches] $ \_ -> do
    batch <- for [1..numPoints] $ \_ -> do
      !time <- (-)
        <$> getPOSIXTime
        <*> (fromIntegral <$> uniformR (0, oneWeekInSeconds) gen)
      !value <- uniform gen
      return (time, value)
    writeBatch wparams $ flip map batch $ \(time, value) ->
      Line ct1
        (Map.fromList [])
        (Map.fromList [("value", nameToFVal value)])
        (Just time)

  queryChunked qparams Default (F.formatQuery ("SELECT * FROM "%F.measurement) ct1) $
    L.mapM_ $ traverse_ $ \Row {..} ->
      printf "%s:\t%s\n"
        (show $ posixSecondsToUTCTime rowTime)
        (show rowValue)

managerSettings :: HC.ManagerSettings
managerSettings = HC.defaultManagerSettings

data Row = Row
  { rowTime :: POSIXTime
  , rowValue :: Name
  } deriving Show

instance QueryResults Row where
  parseMeasurement prec _ _ columns fields = do
    rowTime <- getField "time" columns fields >>= parsePOSIXTime prec
    String name <- getField "value" columns fields
    rowValue <- case name of
      "foo" -> return Foo
      "bar" -> return Bar
      "baz" -> return Baz
      "quu" -> return Quu
      "qux" -> return Qux
      _ -> fail $ "unknown name: " ++ show name
    return Row {..}

data Name
  = Foo
  | Bar
  | Baz
  | Quu
  | Qux
  deriving (Enum, Bounded, Show)

nameToFVal :: Name -> LineField
nameToFVal = FieldString . T.toLower . T.pack . show

instance Variate Name where
  uniform = uniformR (minBound, maxBound)
  uniformR (lower, upper) g = do
    name <- uniformR (fromEnum lower, fromEnum upper) g
    return $! toEnum name
