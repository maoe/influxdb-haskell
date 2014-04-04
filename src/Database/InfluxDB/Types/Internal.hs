{-# LANGUAGE CPP #-}
module Database.InfluxDB.Types.Internal
  ( stripPrefixOptions
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
  { fieldLabelModifier = stripPrefix name
  }
#else
stripPrefixOptions :: String -> String -> String
stripPrefixOptions = stripPrefix
#endif

stripPrefix :: String -> String -> String
stripPrefix prefix xs = case drop (length prefix) xs of
  [] -> error "Insufficient length of field name"
  c:cs -> toLower c : cs
