{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.InfluxDB.Format
  ( Query
  , fromQuery

  , Format
  , (%)
  , formatQuery
  , formatDatabase
  , formatKey

  , fdatabase
  , fkey
  , fval
  , fdecimal
  , frealFloat
  , ftext
  ) where
import Control.Category
import Data.Monoid
import Data.String
import Prelude hiding ((.), id)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Builder as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import qualified Data.Text.Lazy.Builder.Int as TL
import qualified Data.Text.Lazy.Builder.RealFloat as TL

import Database.InfluxDB.Types

fromQuery :: Query -> B.ByteString
fromQuery (Query q) =
  BL.toStrict $ BL.toLazyByteString $ T.encodeUtf8Builder q

newtype Format a b = Format { runFormat :: (TL.Builder -> a) -> b }

instance Category Format where
  id = Format (\k -> k "")
  fmt1 . fmt2 = Format $ \k ->
    runFormat fmt1 $ \a ->
      runFormat fmt2 $ \b ->
        k (a <> b)

instance a ~ b => IsString (Format a b) where
  fromString xs = Format $ \k -> k $ fromString xs

(%) :: Format b c -> Format a b -> Format a c
(%) = (.)

formatQuery :: Format Query r -> r
formatQuery fmt = runFormat fmt (Query . TL.toStrict . TL.toLazyText)

formatDatabase :: Format Database r -> r
formatDatabase fmt = runFormat fmt (Database . TL.toStrict . TL.toLazyText)

formatKey :: Format Key r -> r
formatKey fmt = runFormat fmt (Key . TL.toStrict . TL.toLazyText)

makeFormat :: (a -> TL.Builder) -> Format r (a -> r)
makeFormat build = Format $ \k a -> k $ build a

fdatabase :: Format r (Database -> r)
fdatabase = makeFormat $ \(Database name) -> "\"" <> TL.fromText name <> "\""

fkey :: Format r (Key -> r)
fkey = makeFormat $ \(Key name) -> "\"" <> TL.fromText name <> "\""

fval :: Format r (FieldValue -> r)
fval = makeFormat $ \case
  FieldInt n -> TL.decimal n
  FieldFloat d -> TL.realFloat d
  FieldString s -> "'" <> TL.fromText s <> "'"
  FieldBool b -> if b then "true" else "false"
  FieldNull -> "null"

fdecimal :: Integral a => Format r (a -> r)
fdecimal = makeFormat TL.decimal

frealFloat :: RealFloat a => Format r (a -> r)
frealFloat = makeFormat TL.realFloat

ftext :: Format r (T.Text -> r)
ftext = makeFormat TL.fromText
