{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Database.InfluxDB.JSON
  ( -- * Result parsers
    parseResultsWith
  , parseResultsWithDecoder

  -- ** Decoder settings
  , Decoder(..)
  , strictDecoder
  , lenientDecoder

  -- * Getting fields and tags
  , getField
  , getTag

  -- * Common JSON object parsers
  , A.parseJSON
  , parseUTCTime
  , parsePOSIXTime
  , parseRFC3339
  , parseQueryField
  -- ** Utility functions
  , parseResultsObject
  , parseSeriesObject
  , parseSeriesBody
  , parseErrorObject
  ) where
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Maybe

import Data.Aeson
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format
import Data.Vector (Vector)
import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Scientific as Sci
import qualified Data.Text as T
import qualified Data.Vector as V

import Database.InfluxDB.Types

-- | Parse a JSON response with the 'lenientDecoder'. This can be useful to
-- implement the 'Database.InfluxDB.Query.parseResults' method.
parseResultsWith
  :: (Maybe Text -> HashMap Text Text -> Vector Text -> Array -> A.Parser a)
  -- ^ A parser that takes
  --
  -- 1. an optional name of the series
  -- 2. a map of tags
  -- 3. an array of field names
  -- 4. an array of values
  --
  -- to construct a value.
  -> Value
  -> A.Parser (Vector a)
parseResultsWith = parseResultsWithDecoder lenientDecoder

-- | Parse a JSON response with the specified decoder settings.
parseResultsWithDecoder
  :: Decoder a
  -> (Maybe Text -> HashMap Text Text -> Vector Text -> Array -> A.Parser a)
  -- ^ A parser that takes
  --
  -- 1. an optional name of the series
  -- 2. a map of tags
  -- 3. an array of field names
  -- 4. an array of values
  --
  -- to construct a value.
  -> Value
  -> A.Parser (Vector a)
parseResultsWithDecoder Decoder {..} row val0 = success
  where
    success = do
      results <- parseResultsObject val0

      (join -> series) <- V.forM results $ \val ->
        parseSeriesObject val <|> parseErrorObject val
      values <- V.forM series $ \val -> do
        (name, tags, columns, values) <- parseSeriesBody val
        decodeFold $ V.forM values $ A.withArray "values" $ \fields -> do
          assert (V.length columns == V.length fields) $ return ()
          decodeEach $ row name tags columns fields
      return $! join values

-- | Decoder settings
data Decoder a = forall b. Decoder
  { decodeEach :: A.Parser a -> A.Parser b
  -- ^ How to decode each row. For example 'optional' can be used to turn parse
  -- failrues into 'Nothing's.
  , decodeFold :: A.Parser (Vector b) -> A.Parser (Vector a)
  -- ^ How to aggregate rows into the resulting vector.
  }

-- | A decoder that fails immediately if there's any parse failure.
strictDecoder :: Decoder a
strictDecoder = Decoder
  { decodeEach = id
  , decodeFold = id
  }

-- | A decoder that ignores parse failures and returns only successful results.
lenientDecoder :: Decoder a
lenientDecoder = Decoder
  { decodeEach = optional
  , decodeFold = \p -> do
    bs <- p
    return $! V.map fromJust $ V.filter isJust bs
  }

-- | Get a field value from a column name
getField
  :: Monad m
  => Text -- ^ Column name
  -> Vector Text -- ^ Columns
  -> Vector Value -- ^ Field values
  -> m Value
getField column columns fields =
  case V.elemIndex column columns of
    Nothing -> fail $ "getField: no such column " ++ show column
    Just idx -> case V.indexM fields idx of
      Nothing -> fail $ "getField: index out of bound for " ++ show column
      Just field -> return field

-- | Get a tag value from a tag name
getTag
  :: Monad m
  => Text -- ^ Tag name
  -> HashMap Text Value -- ^ Tags
  -> m Value
getTag tag tags = case HashMap.lookup tag tags of
  Nothing -> fail $ "getTag: no such tag " ++ show tag
  Just val -> return val

-- | Parse a result response.
parseResultsObject :: Value -> A.Parser (Vector A.Value)
parseResultsObject = A.withObject "results" $ \obj -> obj .: "results"

-- | Parse a series response.
parseSeriesObject :: Value -> A.Parser (Vector A.Value)
parseSeriesObject = A.withObject "series" $ \obj ->
  fromMaybe V.empty <$> obj .:? "series"

-- | Parse the common JSON structure used in query responses.
parseSeriesBody
  :: Value
  -> A.Parser (Maybe Text, HashMap Text Text, Vector Text, Array)
parseSeriesBody = A.withObject "series" $ \obj -> do
  !name <- obj .:? "name"
  !columns <- obj .: "columns"
  !values <- obj .:? "values" .!= V.empty
  !tags <- obj .:? "tags" .!= HashMap.empty
  return (name, tags, columns, values)

-- | Parse the common JSON structure used in failure response.
parseErrorObject :: A.Value -> A.Parser a
parseErrorObject = A.withObject "error" $ \obj -> do
  message <- obj .: "error"
  fail $ T.unpack message

-- | Parse either a POSIX timestamp or RFC3339 formatted timestamp as 'UTCTime'.
parseUTCTime :: Precision ty -> A.Value -> A.Parser UTCTime
parseUTCTime prec val = case prec of
  RFC3339 -> parseRFC3339 val
  _ -> posixSecondsToUTCTime <$!> parsePOSIXTime prec val

-- | Parse either a POSIX timestamp or RFC3339 formatted timestamp as
-- 'POSIXTime'.
parsePOSIXTime :: Precision ty -> A.Value -> A.Parser POSIXTime
parsePOSIXTime prec val = case prec of
  RFC3339 -> utcTimeToPOSIXSeconds <$!> parseRFC3339 val
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
parseRFC3339 :: ParseTime time => A.Value -> A.Parser time
parseRFC3339 val = A.withText err
  (maybe (A.typeMismatch err val) (return $!)
    . parseTimeM True defaultTimeLocale fmt
    . T.unpack)
  val
  where
    fmt, err :: String
    fmt = "%FT%X%QZ"
    err = "RFC3339-formatted timestamp"

-- | Parse a 'QueryField'.
parseQueryField :: A.Value -> A.Parser QueryField
parseQueryField val = case val of
  A.Number sci ->
    return $! either FieldFloat FieldInt $ Sci.floatingOrInteger sci
  A.String txt ->
    return $! FieldString txt
  A.Bool b ->
    return $! FieldBool b
  A.Null ->
    return FieldNull
  _ -> fail $ "parseQueryField: expected a flat data structure, but got "
    ++ show val
{-# DEPRECATED parseQueryField
  "This function parses numbers in a misleading way. Use 'parseJSON' instead."
  #-}
