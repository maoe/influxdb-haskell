{-# LANGUAGE CPP #-}
module Database.InfluxDB.Types.Internal
  ( stripPrefixOptions
  , stripPrefixLower
  ) where
import Data.Char (toLower)

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

stripPrefixLower :: String -> String -> String
stripPrefixLower prefix xs = case drop (length prefix) xs of
  [] -> error "Insufficient length of field name"
  c:cs -> toLower c : cs
