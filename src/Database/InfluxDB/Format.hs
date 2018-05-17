{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.InfluxDB.Format
  ( -- * The 'Format' type and associated functions
    Format
  , makeFormat
  , (%)

  -- * Formatting functions
  , formatQuery
  , formatDatabase
  , formatMeasurement
  , formatKey

  -- * Formatters for various types
  , database
  , key
  , keys
  , measurement
  , measurements
  , field
  , decimal
  , realFloat
  , text
  , string
  , byteString8
  , time

  -- * Utility functions
  , fromQuery
  ) where
import Control.Category
import Data.Monoid
import Data.String
import Prelude hiding ((.), id)

import Data.Time
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Builder as BL
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import qualified Data.Text.Lazy.Builder.Int as TL
import qualified Data.Text.Lazy.Builder.RealFloat as TL

import Database.InfluxDB.Internal.Text
import Database.InfluxDB.Types hiding (database)

-- $setup
-- >>> :set -XOverloadedStrings

-- | Serialize a 'Query' to a 'B.ByteString'.
fromQuery :: Query -> B.ByteString
fromQuery (Query q) =
  BL.toStrict $ BL.toLazyByteString $ T.encodeUtf8Builder q

-- | A typed format string. @Format a r@ means that @a@ is the type of formatted
-- string, and @r@ is the type of the formatter.
--
-- >>> :t formatQuery
-- formatQuery :: Format Query r -> r
-- >>> :t key
-- key :: Format r (Key -> r)
-- >>> :t "SELECT * FROM "%key
-- "SELECT * FROM "%key :: Format a (Key -> a)
-- >>> :t formatQuery ("SELECT * FROM "%key)
-- formatQuery ("SELECT * FROM "%key) :: Key -> Query
-- >>> formatQuery ("SELECT * FROM "%key) "series"
-- "SELECT * FROM \"series\""
newtype Format a r = Format { runFormat :: (TL.Builder -> a) -> r }

-- | 'Format's can be composed using @('.')@ from "Control.Category".
--
-- >>> import Control.Category ((.))
-- >>> import Prelude hiding ((.))
-- >>> formatQuery ("SELECT * FROM " . key) "series"
-- "SELECT * FROM \"series\""
instance Category Format where
  id = Format (\k -> k "")
  fmt1 . fmt2 = Format $ \k ->
    runFormat fmt1 $ \a ->
      runFormat fmt2 $ \b ->
        k (a <> b)

-- | With the OverloadedStrings exension, string literals can be used to write
-- queries.
--
-- >>> "SELECT * FROM series" :: Query
-- "SELECT * FROM series"
instance a ~ r => IsString (Format a r) where
  fromString xs = Format $ \k -> k $ fromString xs

-- | 'Format' specific synonym of @('.')@.
--
-- This is typically easier to use than @('.')@ is because it doesn't
-- conflict with @Prelude.(.)@.
(%) :: Format b c -> Format a b -> Format a c
(%) = (.)

runFormatWith :: (T.Text -> a) -> Format a r -> r
runFormatWith f fmt = runFormat fmt (f . TL.toStrict . TL.toLazyText)

-- | Format a 'Query'.
--
-- >>> formatQuery "SELECT * FROM series"
-- "SELECT * FROM series"
-- >>> formatQuery ("SELECT * FROM "%key) "series"
-- "SELECT * FROM \"series\""
formatQuery :: Format Query r -> r
formatQuery = runFormatWith Query

-- | Format a 'Database'.
--
-- >>> formatDatabase "test-db"
-- "test-db"
formatDatabase :: Format Database r -> r
formatDatabase = runFormatWith Database

-- | Format a 'Measurement'.
--
-- >>> formatMeasurement "test-series"
-- "test-series"
formatMeasurement :: Format Measurement r -> r
formatMeasurement = runFormatWith Measurement

-- | Format a 'Key'.
--
-- >>> formatKey "test-key"
-- "test-key"
formatKey :: Format Key r -> r
formatKey fmt = runFormat fmt (Key . TL.toStrict . TL.toLazyText)

-- | Convenience function to make a custom formatter.
makeFormat :: (a -> TL.Builder) -> Format r (a -> r)
makeFormat build = Format $ \k a -> k $ build a

doubleQuote :: T.Text -> TL.Builder
doubleQuote name = "\"" <> TL.fromText name <> "\""

singleQuote :: T.Text -> TL.Builder
singleQuote name = "'" <> TL.fromText name <> "'"

identifierBuilder :: T.Text -> TL.Builder
identifierBuilder = doubleQuote . escapeDoubleQuotes

stringBuilder :: T.Text -> TL.Builder
stringBuilder = singleQuote . escapeSingleQuotes

-- | Format a database name.
--
-- >>> formatQuery ("CREATE DATABASE "%database) "test-db"
-- "CREATE DATABASE \"test-db\""
database :: Format r (Database -> r)
database = makeFormat $ \(Database name) -> identifierBuilder name

-- | Format an identifier (e.g. field names, tag names, etc).
--
-- Identifiers in InfluxDB protocol are surrounded with double quotes.
--
-- >>> formatQuery ("SELECT "%key%" FROM series") "field"
-- "SELECT \"field\" FROM series"
-- >>> formatQuery ("SELECT "%key%" FROM series") "foo\"bar"
-- "SELECT \"foo\\\"bar\" FROM series"
key :: Format r (Key -> r)
key = makeFormat $ \(Key name) -> identifierBuilder name

-- | Format multiple keys.
--
-- >>> formatQuery ("SELECT "%keys%" FROM series") ["field1", "field2"]
-- "SELECT \"field1\",\"field2\" FROM series"
keys :: Format r ([Key] -> r)
keys = makeFormat $
  mconcat . L.intersperse "," . map (\(Key name) -> identifierBuilder name)

-- | Format a measurement.
--
-- >>> formatQuery ("SELECT * FROM "%measurement) "test-series"
-- "SELECT * FROM \"test-series\""
measurement :: Format r (Measurement -> r)
measurement = makeFormat $ \(Measurement name) -> identifierBuilder name

-- | Format a measurement.
--
-- >>> formatQuery ("SELECT * FROM "%measurements) ["series1", "series2"]
-- "SELECT * FROM \"series1\",\"series2\""
measurements :: Format r ([Measurement] -> r)
measurements = makeFormat $
  mconcat . L.intersperse ","
    . map (\(Measurement name) -> identifierBuilder name)

-- | Format an InfluxDB value. Good for field and tag values.
--
-- >>> formatQuery ("SELECT * FROM series WHERE "%key%" = "%field) "location" "tokyo"
-- "SELECT * FROM series WHERE \"location\" = 'tokyo'"
field :: Format r (QueryField -> r)
field = makeFormat $ \case
  FieldInt n -> TL.decimal n
  FieldFloat d -> TL.realFloat d
  FieldString s -> stringBuilder s
  FieldBool b -> if b then "true" else "false"
  FieldNull -> "null"

-- | Format a decimal number.
--
-- >>> formatQuery ("SELECT * FROM series WHERE time < now() - "%decimal%"h") 1
-- "SELECT * FROM series WHERE time < now() - 1h"
decimal :: Integral a => Format r (a -> r)
decimal = makeFormat TL.decimal

-- | Format a floating-point number.
--
-- >>> formatQuery ("SELECT * FROM series WHERE value > "%realFloat) 0.1
-- "SELECT * FROM series WHERE value > 0.1"
realFloat :: RealFloat a => Format r (a -> r)
realFloat = makeFormat TL.realFloat

-- | Format a text.
--
-- Note that this doesn't escape the string. Use 'fieldKey' to format field
-- values in a query.
--
-- >>> :t formatKey text
-- formatKey text :: T.Text -> Key
text :: Format r (T.Text -> r)
text = makeFormat TL.fromText

-- | Format a string.
--
-- Note that this doesn't escape the string. Use 'fieldKey' to format field
-- values in a query.
--
-- >>> :t formatKey string
-- formatKey string :: String -> Key
string :: Format r (String -> r)
string = makeFormat TL.fromString

-- | Format a UTF-8 encoded byte string.
--
-- Note that this doesn't escape the string. Use 'fieldKey' to format field
-- values in a query.
--
-- >>> :t formatKey byteString8
-- formatKey byteString8 :: B.ByteString -> Key
byteString8 :: Format r (B.ByteString -> r)
byteString8 = makeFormat $ TL.fromText . T.decodeUtf8

-- | Format a time.
--
-- >>> import Data.Time
-- >>> let Just t = parseTimeM False defaultTimeLocale "%s" "0" :: Maybe UTCTime
-- >>> formatQuery ("SELECT * FROM series WHERE time >= "%time) t
-- "SELECT * FROM series WHERE time >= '1970-01-01 00:00:00'"
time :: FormatTime time => Format r (time -> r)
time = makeFormat $ \t ->
  "'" <> TL.fromString (formatTime defaultTimeLocale fmt t) <> "'"
  where
    fmt = "%F %X%Q" -- YYYY-MM-DD HH:MM:SS.nnnnnnnnn
