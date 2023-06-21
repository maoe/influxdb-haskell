{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Database.InfluxDB.JSON
  ( -- * Result parsers
    parseResultsWith
  , parseResultsWithDecoder

  -- ** Decoder settings
  , Decoder(..)
  , SomeDecoder(..)
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
  -- ** Utility functions
  , parseResultsObject
  , parseSeriesObject
  , parseSeriesBody
  , parseErrorObject
  ) where
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Foldable
import Data.Maybe
import Prelude
import qualified Control.Monad.Fail as Fail

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

-- $setup
-- >>> import Data.Maybe
-- >>> import Data.Aeson (decode)
-- >>> import Database.InfluxDB.JSON
-- >>> import qualified Data.Aeson.Types as A

-- | Parse a JSON response with the 'strictDecoder'.
parseResultsWith
  :: (Maybe Text -> HashMap Text Text -> Vector Text -> Array -> A.Parser a)
  -- ^ A parser that parses a measurement. A measurement consists of
  --
  -- 1. an optional name of the series
  -- 2. a map of tags
  -- 3. an array of field keys
  -- 4. an array of field values
  -> Value -- ^ JSON response
  -> A.Parser (Vector a)
parseResultsWith = parseResultsWithDecoder strictDecoder

-- | Parse a JSON response with the specified decoder settings.
parseResultsWithDecoder
  :: Decoder
  -> (Maybe Text -> HashMap Text Text -> Vector Text -> Array -> A.Parser a)
  -- ^ A parser that parses a measurement. A measurement consists of
  --
  -- 1. an optional name of the series
  -- 2. a map of tags
  -- 3. an array of field keys
  -- 4. an array of field values
  -> Value -- ^ JSON response
  -> A.Parser (Vector a)
parseResultsWithDecoder (Decoder SomeDecoder {..}) row val0 = do
  r <- foldr1 (<|>)
    [ Left <$> parseErrorObject val0
    , Right <$> success
    ]
  case r of
    Left err -> fail err
    Right vec -> return vec
  where
    success = do
      results <- parseResultsObject val0

      (join -> series) <- V.forM results $ \val -> do
        r <- foldr1 (<|>)
          [ Left <$> parseErrorObject val
          , Right <$> parseSeriesObject val
          ]
        case r of
          Left err -> fail err
          Right vec -> return vec
      values <- V.forM series $ \val -> do
        (name, tags, columns, values) <- parseSeriesBody val
        decodeFold $ V.forM values $ A.withArray "values" $ \fields -> do
          assert (V.length columns == V.length fields) $ return ()
          decodeEach $ row name tags columns fields
      return $! join values

-- | A decoder to use when parsing a JSON response.
--
-- Use 'strictDecoder' if you want to fail the entire decoding process if
-- there's any failure. Use 'lenientDecoder' if you want the decoding process
-- to collect only successful results.
newtype Decoder = Decoder (forall a. SomeDecoder a)

-- | @'SomeDecoder' a@ represents how to decode a JSON response given a row
-- parser of type @'A.Parser' a@.
data SomeDecoder a = forall b. SomeDecoder
  { decodeEach :: A.Parser a -> A.Parser b
  -- ^ How to decode each row.
  --
  -- For example 'optional' can be used to turn parse
  -- failrues into 'Nothing's.
  , decodeFold :: A.Parser (Vector b) -> A.Parser (Vector a)
  -- ^ How to aggregate rows into the resulting vector.
  --
  -- For example when @b ~ 'Maybe' a@, one way to aggregate the values is to
  -- return only 'Just's.
  }

-- | A decoder that fails immediately if there's any parse failure.
--
-- 'strictDecoder' is defined as follows:
--
-- @
-- strictDecoder :: Decoder
-- strictDecoder = Decoder $ SomeDecoder
--  { decodeEach = id
--  , decodeFold = id
--  }
-- @
strictDecoder :: Decoder
strictDecoder = Decoder $ SomeDecoder
  { decodeEach = id
  , decodeFold = id
  }

-- | A decoder that ignores parse failures and returns only successful results.
lenientDecoder :: Decoder
lenientDecoder = Decoder $ SomeDecoder
  { decodeEach = optional
  , decodeFold = \p -> do
    bs <- p
    return $! V.map fromJust $ V.filter isJust bs
  }

-- | Get a field value from a column name
getField
  :: Fail.MonadFail m
  => Text -- ^ Column name
  -> Vector Text -- ^ Columns
  -> Vector Value -- ^ Field values
  -> m Value
getField column columns fields =
  case V.elemIndex column columns of
    Nothing -> Fail.fail $ "getField: no such column " ++ show column
    Just idx -> case V.indexM fields idx of
      Nothing -> Fail.fail $ "getField: index out of bound for " ++ show column
      Just field -> return field

-- | Get a tag value from a tag name
getTag
  :: Fail.MonadFail m
  => Text -- ^ Tag name
  -> HashMap Text Value -- ^ Tags
  -> m Value
getTag tag tags = case HashMap.lookup tag tags of
  Nothing -> Fail.fail $ "getTag: no such tag " ++ show tag
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
-- >>> A.parse parseErrorObject $ fromJust $ decode "{ \"error\": \"custom error\" }"
-- Success "custom error"
-- >>> A.parse parseErrorObject $ fromJust $ decode "{ \"message\": \"custom error\" }"
-- Success "custom error"
parseErrorObject :: A.Value -> A.Parser String
parseErrorObject = A.withObject "error" $ \obj -> obj .: "error" <|> obj .: "message"

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
-- Note that this parser is slow as it converts a 'T.Text' input to a
-- 'Prelude.String' before parsing.
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
