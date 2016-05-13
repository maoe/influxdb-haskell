{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module Database.InfluxDB.JSON
  ( parseResultsWith
  , resultsObject
  , seriesObject
  , columnsValuesObject
  , errorObject
  , parseTimestamp
  , parsePOSIXTime
  , parseRFC3339
  ) where
import Control.Applicative hiding (optional)
import Control.Exception
import Control.Monad

import Data.Aeson
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format
import Data.Vector (Vector)
import qualified Data.Aeson.Types as A
import qualified Data.Scientific as Sci
import qualified Data.Text as T
import qualified Data.Vector as V

import Database.InfluxDB.Types

parseResultsWith
  :: (Array -> Array -> A.Parser a)
  -- ^ A parser that constucts a value from an array of field names and field
  -- values
  -> Value
  -> A.Parser (Vector a)
parseResultsWith row val0 = success <|> errorObject val0
  where
    success = do
      results <- resultsObject val0
      (join -> series) <- V.forM results $ \val ->
        seriesObject val <|> errorObject val
      values <- V.forM series $ \val -> do
        (columns, values) <- columnsValuesObject val
        V.forM values $ A.withArray "values" $ \fields -> do
          assert (V.length columns == V.length fields) $ return ()
          row columns fields
      return $! join values

resultsObject :: Value -> A.Parser (Vector A.Value)
resultsObject = A.withObject "results" $ \obj -> do
  Array results <- obj .: "results"
  return results

seriesObject :: Value -> A.Parser (Vector A.Value)
seriesObject = A.withObject "series" $ \obj -> do
  Array series <- obj .: "series"
  return series

columnsValuesObject :: Value -> A.Parser (Array, Array)
columnsValuesObject = A.withObject "columns/values" $ \obj -> do
  Array columns <- obj .: "columns"
  Array values <- obj .: "values"
  return (columns, values)

errorObject :: A.Value -> A.Parser a
errorObject = A.withObject "error" $ \obj -> do
  String message <- obj .: "error"
  fail $ T.unpack message

-- | Parse either a POSIX timestamp or RFC3339 formatted timestamp.
parseTimestamp :: Precision ty -> A.Value -> A.Parser POSIXTime
parseTimestamp prec val = case prec of
  RFC3339 -> utcTimeToPOSIXSeconds <$!> parseRFC3339 val
  _ -> parsePOSIXTime prec val

-- | Parse an integer POSIX timestamp in given time precision.
parsePOSIXTime :: Precision ty -> A.Value -> A.Parser POSIXTime
parsePOSIXTime prec val = case prec of
  RFC3339 -> A.typeMismatch err val
  _ -> A.withScientific err
    (\s -> case timestampToUTC s of
      Nothing -> A.typeMismatch err val
      Just !utc -> return utc)
    val
  where
    err = "POSIX timestamp in " ++ T.unpack (precisionName prec)
    timestampToUTC s = do
      n <- Sci.toBoundedInteger s
      return $! fromIntegral (n :: Int) * precisionScale prec

-- | Parse a RFC3339-formatted timestamp.
--
-- Note that this parser is slow as it converts a 'T.Text' input to a 'String'
-- before parsing.
parseRFC3339 :: A.Value -> A.Parser UTCTime
parseRFC3339 val = A.withText err
  (\text -> maybe (A.typeMismatch err val) (return $!) $
    parseTimeM True defaultTimeLocale fmt $ T.unpack text)
  val
  where
    fmt, err :: String
    fmt = "%FT%X%QZ"
    err = "RFC3339-formatted timestamp"
