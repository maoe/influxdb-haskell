module Database.InfluxDB.Types.Internal
  ( stripPrefixOptions
  ) where
import Data.Char (toLower)

import Data.Aeson.TH

stripPrefixOptions :: String -> Options
stripPrefixOptions name = defaultOptions
  { fieldLabelModifier = modifier
  }
  where
    modifier xs = case drop (length name) xs of
      [] -> error "Insufficient length of field name"
      c:cs -> toLower c:cs
