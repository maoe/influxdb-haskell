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
  , formatKey

  -- * Formatters for various types
  , database
  , key
  , keys
  , fieldVal
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

-- | Format a 'Query'.
--
-- >>> formatQuery "SELECT * FROM series"
-- "SELECT * FROM series"
-- >>> formatQuery ("SELECT * FROM "%key) "series"
-- "SELECT * FROM \"series\""
formatQuery :: Format Query r -> r
formatQuery fmt = runFormat fmt (Query . TL.toStrict . TL.toLazyText)

-- | Format a 'Database'.
--
-- >>> formatDatabase "test-db"
-- "test-db"
formatDatabase :: Format Database r -> r
formatDatabase fmt = runFormat fmt (Database . TL.toStrict . TL.toLazyText)

-- | Format a 'Key'.
--
-- >>> formatKey "test-key"
-- "test-key"
formatKey :: Format Key r -> r
formatKey fmt = runFormat fmt (Key . TL.toStrict . TL.toLazyText)

-- | Convenience function to make a custom formatter.
makeFormat :: (a -> TL.Builder) -> Format r (a -> r)
makeFormat build = Format $ \k a -> k $ build a

-- | Format a database name.
--
-- >>> formatQuery ("CREATE DATABASE "%database) "test-db"
-- "CREATE DATABASE \"test-db\""
database :: Format r (Database -> r)
database = makeFormat $ \(Database name) -> "\"" <> TL.fromText name <> "\""

keyBuilder :: Key -> TL.Builder
keyBuilder (Key name) = "\"" <> TL.fromText name <> "\""

-- | Format a key (e.g. series names, field names etc).
--
-- >>> formatQuery ("SELECT * FROM "%key) "test-series"
-- "SELECT * FROM \"test-series\""
key :: Format r (Key -> r)
key = makeFormat keyBuilder

-- | Format multiple keys.
--
-- >>> formatQuery ("SELECT "%keys%" FROM series") ["field1", "field2"]
-- "SELECT \"field1\",\"field2\" FROM series"
keys :: Format r ([Key] -> r)
keys = makeFormat $ mconcat . L.intersperse "," . map keyBuilder

-- | Format 'QueryField'.
--
-- >>> formatQuery ("SELECT * FROM series WHERE "%key%" = "%fieldVal) "location" "tokyo"
-- "SELECT * FROM series WHERE \"location\" = 'tokyo'"
fieldVal :: Format r (QueryField -> r)
fieldVal = makeFormat $ \case
  FieldInt n -> TL.decimal n
  FieldFloat d -> TL.realFloat d
  FieldString s -> "'" <> TL.fromText s <> "'"
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
