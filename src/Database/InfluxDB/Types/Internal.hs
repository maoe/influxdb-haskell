{-# LANGUAGE CPP #-}
module Database.InfluxDB.Types.Internal
  ( stripPrefixOptions
  , stripPrefixLower
  , stripPrefixSnake
  ) where
import Data.Char (isUpper, toLower)

-------------------------------------------------
-- Conditional imports

#if MIN_VERSION_aeson(0, 6, 2)
import Data.Aeson.TH (Options(..), defaultOptions)
#endif

-------------------------------------------------

#if MIN_VERSION_aeson(0, 6, 2)
stripPrefixOptions :: String -> Options
stripPrefixOptions name = defaultOptions
  { fieldLabelModifier = stripPrefixLower name
  }
#else
stripPrefixOptions :: String -> String -> String
stripPrefixOptions = stripPrefixLower
#endif

-- | Strip the prefix then convert to 'lowerCamelCase'.
stripPrefixLower
  :: String -- ^ Prefix to be stripped
  -> String -- ^ Input string
  -> String
stripPrefixLower prefix xs = case drop (length prefix) xs of
  [] -> error "Insufficient length of field name"
  c:cs -> toLower c : cs

-- | Strip the prefix then convert to 'snake_case'.
stripPrefixSnake
  :: String -- ^ Prefix to be stripped
  -> String -- ^ Input string
  -> String
stripPrefixSnake prefix xs = case drop (length prefix) xs of
  [] -> error "Insufficient length of field name"
  cs -> toSnake cs
  where
    toSnake = dropWhile (== '_') . foldr f []
    f c cs
      | isUpper c = '_':toLower c:cs
      | otherwise = c:cs
