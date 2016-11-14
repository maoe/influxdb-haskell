{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module Database.InfluxDB.JSON
  ( parseResultsWith
  , parseResultsWithDecoder
  , Decoder(..)
  , strictDecoder
  , lenientDecoder

  , getField
  , getTag

  , parseTimestamp
  , parsePOSIXTime
  , parseRFC3339
  , parseFieldValue

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
import Data.Text (Text)
import Data.Time.Clock.POSIX
import Data.Time.Format
import Data.Vector (Vector)
import qualified Data.Aeson.Types as A
import qualified Data.Scientific as Sci
import qualified Data.Text as T
import qualified Data.Vector as V

import Database.InfluxDB.Types

parseResultsWith
  :: (Maybe Text -> Vector Text -> Array -> Array -> A.Parser a)
  -- ^ A parser that takes
  --
  -- 1. an optional name of the series
  -- 2. an array of tags
  -- 3. an array of field names
  -- 4. an array of values
  --
  -- to construct a value.
  -> Value
  -> A.Parser (Vector a)
parseResultsWith = parseResultsWithDecoder lenientDecoder

parseResultsWithDecoder
  :: Decoder a
  -> (Maybe Text -> Vector Text -> Array -> Array -> A.Parser a)
  -- ^ A parser that takes
  --
  -- 1. an optional name of the series
  -- 2. an array of tags
  -- 3. an array of field names
  -- 4. an array of values
  --
  -- to construct a value.
  -> Value
  -> A.Parser (Vector a)
parseResultsWithDecoder Decoder {..} row val0 = success <|> parseErrorObject val0
  where
    success = do
      results <- parseResultsObject val0
      if results == V.fromList [A.emptyObject]
        then return V.empty
        else do
          (join -> series) <- V.forM results $ \val ->
            parseSeriesObject val <|> parseErrorObject val
          values <- V.forM series $ \val -> do
            (name, tags, columns, values) <- parseSeriesBody val
            decodeFold $ V.forM values $ A.withArray "values" $ \fields -> do
              assert (V.length columns == V.length fields) $ return ()
              decodeEach $ row name tags columns fields
          return $! join values

data Decoder a = forall b. Decoder
  { decodeEach :: A.Parser a -> A.Parser b
  , decodeFold :: A.Parser (Vector b) -> A.Parser (Vector a)
  }

strictDecoder :: Decoder a
strictDecoder = Decoder
  { decodeEach = id
  , decodeFold = id
  }

lenientDecoder :: Decoder a
lenientDecoder = Decoder
  { decodeEach = optional
  , decodeFold = \p -> do
    bs <- p
    return $! V.map fromJust $ V.filter isJust bs
  }

-- | Get a field value which corresponds to a given column name
getField
  :: T.Text -- ^ Column name
  -> Array -- ^ Columns
  -> Array -- ^ Fields
  -> A.Parser Value
getField (A.String -> column) columns fields =
  case V.elemIndex column columns of
    Nothing -> fail $ "getField: no such column " ++ show column
    Just idx -> case V.indexM fields idx of
      Nothing -> fail $ "getField: index out of bound for " ++ show column
      Just field -> return field

getTag
  :: T.Text -- ^ Tag name
  -> Array -- ^ Tags
  -> A.Parser T.Text
getTag (A.String -> tag) tags =
  case V.find (== tag) tags of
    Nothing -> fail $ "getTag: no such tag " ++ show tag
    Just val -> A.withText "tag" return val

parseResultsObject :: Value -> A.Parser (Vector A.Value)
parseResultsObject = A.withObject "results" $ \obj -> do
  Array results <- obj .: "results"
  return results

parseSeriesObject :: Value -> A.Parser (Vector A.Value)
parseSeriesObject = A.withObject "series" $ \obj -> do
  Array series <- obj .: "series"
  return series

parseSeriesBody :: Value -> A.Parser (Maybe Text, Vector Text, Array, Array)
parseSeriesBody = A.withObject "columns/values" $ \obj -> do
  name <- obj .:? "name"
  Array columns <- obj .: "columns"
  Array values <- obj .: "values"
  tags <- obj .:? "tags" .!= empty
  return (name, tags, columns, values)

parseErrorObject :: A.Value -> A.Parser a
parseErrorObject = A.withObject "error" $ \obj -> do
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
parseRFC3339 :: ParseTime time => A.Value -> A.Parser time
parseRFC3339 val = A.withText err
  (\text -> maybe (A.typeMismatch err val) (return $!) $
    parseTimeM True defaultTimeLocale fmt $ T.unpack text)
  val
  where
    fmt, err :: String
    fmt = "%FT%X%QZ"
    err = "RFC3339-formatted timestamp"

parseFieldValue :: A.Value -> A.Parser FieldValue
parseFieldValue val = case val of
  A.Number sci ->
    return $! either FieldFloat FieldInt $ Sci.floatingOrInteger sci
  A.String txt ->
    return $! FieldString txt
  A.Bool b ->
    return $! FieldBool b
  A.Null ->
    return FieldNull
  _ -> fail "parseFieldValue: expected a flat data structure"
