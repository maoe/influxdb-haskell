{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
import Control.Monad
import Data.Function (fix)
import Data.Traversable
import System.Environment
import System.IO
import Text.Printf (printf)

import Control.Lens
import Data.Aeson
import Data.Optional (Optional(Default))
import Data.Time.Clock.POSIX
import System.Random.MWC (Variate(..))
import qualified Control.Foldl as Fold
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Network.HTTP.Client as HC
import qualified System.Random.MWC as MWC

import Database.InfluxDB
import Database.InfluxDB.Format as Q
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

  M.manage qparams $ Q.formatQuery ("DROP DATABASE "%fdatabase) ctx
  M.manage qparams $ Q.formatQuery ("CREATE DATABASE "%fdatabase) ctx

  let wparams = writeParams ctx & manager .~ Right manager'

  gen <- MWC.create
  flip fix batches $ \outerLoop !m -> when (m > 0) $ do
    batch <- for [1..numPoints] $ \_ -> do
      !time <- (-)
        <$> getPOSIXTime
        <*> (fromIntegral <$> uniformR (0, oneWeekInSeconds) gen)
      !value <- uniform gen
      return (time, value)
    writeBatch wparams $ flip map batch $ \(time, value) ->
        Line ct1 [] [("value", nameToFVal value)] (Just time)
    outerLoop $ m - 1

  queryChunked qparams Default (Q.formatQuery ("SELECT * FROM "%fkey) ct1) $
    Fold.mapM_ $ \Row {..} ->
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
  parseResults prec = parseResultsWith $ \columns fields -> do
    case V.elemIndex "time" columns >>= V.indexM fields of
      Nothing -> fail "no timestamp"
      Just time -> do
        rowTime <- parseTimestamp prec time
        maybe err return $ do
          String name <- V.elemIndex "value" columns >>= V.indexM fields
          rowValue <- case name of
            "foo" -> return Foo
            "bar" -> return Bar
            "baz" -> return Baz
            "quu" -> return Quu
            "qux" -> return Qux
            _ -> fail $ "unknown name: " ++ show name
          return Row {..}
    where
      err = fail "failed to parse a Row"

data Name
  = Foo
  | Bar
  | Baz
  | Quu
  | Qux
  deriving (Enum, Bounded, Show)

nameToFVal :: Name -> FieldValue
nameToFVal = FieldString . T.toLower . T.pack . show

instance Variate Name where
  uniform = uniformR (minBound, maxBound)
  uniformR (lower, upper) g = do
    name <- uniformR (fromEnum lower, fromEnum upper) g
    return $! toEnum name
