{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Database.InfluxDB.Line
  ( -- $setup

  -- * Types and accessors
    Line(Line)
  , measurement
  , tagSet
  , fieldSet
  , timestamp

  -- * Serializers
  , buildLine
  , buildLines
  , encodeLine
  , encodeLines

  -- * Other types
  , LineField
  , Field(..)
  , Precision(..)
  ) where
import Data.List (intersperse)
import Data.Int (Int64)
import Data.Monoid
import Prelude

import Control.Lens
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as TE

import Database.InfluxDB.Internal.Text
import Database.InfluxDB.Types

{- $setup
The Line protocol implementation.

>>> :set -XOverloadedStrings
>>> import Data.Time
>>> import Database.InfluxDB.Line
>>> import System.IO (stdout)
>>> import qualified Data.ByteString as B
>>> import qualified Data.ByteString.Builder as B
>>> import qualified Data.ByteString.Lazy.Char8 as BL8
>>> :{
let l1 = Line "cpu_usage"
      (Map.singleton "cpu" "cpu-total")
      (Map.fromList
        [ ("idle",   FieldFloat 10.1)
        , ("system", FieldFloat 53.3)
        , ("user",   FieldFloat 46.6)
        ])
      (Just $ parseTimeOrError False defaultTimeLocale
        "%F %T%Q %Z"
        "2017-06-17 15:41:40.42659044 UTC") :: Line UTCTime
:}
-}

-- | Placeholder for the Line Protocol
--
-- See https://docs.influxdata.com/influxdb/v1.7/write_protocols/line_protocol_tutorial/ for the
-- concrete syntax.
data Line time = Line
  { _measurement :: !Measurement
  -- ^ Measurement name
  , _tagSet :: !(Map Key Key)
  -- ^ Set of tags (optional)
  , _fieldSet :: !(Map Key LineField)
  -- ^ Set of fields
  --
  -- It shouldn't be empty.
  , _timestamp :: !(Maybe time)
  -- ^ Timestamp (optional)
  }

-- | Serialize a 'Line' to a lazy bytestring
--
-- >>> BL8.putStrLn $ encodeLine (scaleTo Second) l1
-- cpu_usage,cpu=cpu-total idle=10.1,system=53.3,user=46.6 1497714100
encodeLine
  :: (time -> Int64)
  -- ^ Function to convert time to an InfluxDB timestamp
  --
  -- Use 'scaleTo' for HTTP writes and 'roundTo' for UDP writes.
  -> Line time
  -> L.ByteString
encodeLine toTimestamp = B.toLazyByteString . buildLine toTimestamp

-- | Serialize 'Line's to a lazy bytestring
--
-- >>> BL8.putStr $ encodeLines (scaleTo Second) [l1, l1]
-- cpu_usage,cpu=cpu-total idle=10.1,system=53.3,user=46.6 1497714100
-- cpu_usage,cpu=cpu-total idle=10.1,system=53.3,user=46.6 1497714100
--
encodeLines
  :: Foldable f
  => (time -> Int64)
  -- ^ Function to convert time to an InfluxDB timestamp
  --
  -- Use 'scaleTo' for HTTP writes and 'roundTo' for UDP writes.
  -> f (Line time)
  -> L.ByteString
encodeLines toTimestamp = B.toLazyByteString . buildLines toTimestamp

-- | Serialize a 'Line' to a bytestring 'B.Buider'
--
-- >>> B.hPutBuilder stdout $ buildLine (scaleTo Second) l1
-- cpu_usage,cpu=cpu-total idle=10.1,system=53.3,user=46.6 1497714100
buildLine
  :: (time -> Int64)
  -> Line time
  -> B.Builder
buildLine toTimestamp Line {..} =
  key <> " " <> fields <> maybe "" (" " <>) timestamp
  where
    measurement = TE.encodeUtf8Builder $ escapeMeasurement _measurement
    tags = buildMap (TE.encodeUtf8Builder . escapeKey) _tagSet
    key = if Map.null _tagSet
      then measurement
      else measurement <> "," <> tags
    fields = buildMap buildFieldValue _fieldSet
    timestamp = B.int64Dec . toTimestamp <$> _timestamp
    buildMap encodeVal =
      mconcat . intersperse "," . map encodeKeyVal . Map.toList
      where
        encodeKeyVal (name, val) = mconcat
          [ TE.encodeUtf8Builder $ escapeKey name
          , "="
          , encodeVal val
          ]

escapeKey :: Key -> Text
escapeKey (Key text) = escapeCommas $ escapeEqualSigns $ escapeSpaces text

escapeMeasurement :: Measurement -> Text
escapeMeasurement (Measurement text) = escapeCommas $ escapeSpaces text

escapeStringField :: Text -> Text
escapeStringField = escapeDoubleQuotes . escapeBackslashes

buildFieldValue :: LineField -> B.Builder
buildFieldValue = \case
  FieldInt i -> B.int64Dec i <> "i"
  FieldFloat d -> B.doubleDec d
  FieldString t -> "\"" <> TE.encodeUtf8Builder (escapeStringField t) <> "\""
  FieldBool b -> if b then "true" else "false"

-- | Serialize 'Line's to a bytestring 'B.Builder'
--
-- >>> B.hPutBuilder stdout $ buildLines (scaleTo Second) [l1, l1]
-- cpu_usage,cpu=cpu-total idle=10.1,system=53.3,user=46.6 1497714100
-- cpu_usage,cpu=cpu-total idle=10.1,system=53.3,user=46.6 1497714100
--
buildLines
  :: Foldable f
  => (time -> Int64)
  -> f (Line time)
  -> B.Builder
buildLines toTimestamp = foldMap ((<> "\n") . buildLine toTimestamp)

makeLensesWith (lensRules & generateSignatures .~ False) ''Line

-- | Name of the measurement that you want to write your data to.
measurement :: Lens' (Line time) Measurement

-- | Tag(s) that you want to include with your data point. Tags are optional in
-- the Line Protocol, so you can set it 'Control.Applicative.empty'.
tagSet :: Lens' (Line time) (Map Key Key)

-- | Field(s) for your data point. Every data point requires at least one field
-- in the Line Protocol, so it shouldn't be 'Control.Applicative.empty'.
fieldSet :: Lens' (Line time) (Map Key LineField)

-- | Timestamp for your data point. You can put whatever type of timestamp that
-- is an instance of the 'Timestamp' class.
timestamp :: Lens' (Line time) (Maybe time)
