{-# LANGUAGE OverloadedStrings #-}
module Database.InfluxDB.Internal.Text
  ( escapeCommas
  , escapeEqualSigns
  , escapeSpaces
  , escapeDoubleQuotes
  , escapeSingleQuotes
  ) where
import Data.Text (Text)
import qualified Data.Text as T

escapeCommas
  , escapeEqualSigns
  , escapeSpaces
  , escapeDoubleQuotes
  , escapeSingleQuotes :: Text -> Text
escapeCommas = T.replace "," "\\,"
escapeEqualSigns = T.replace "=" "\\="
escapeSpaces = T.replace " " "\\ "
escapeDoubleQuotes = T.replace "\"" "\\\""
escapeSingleQuotes = T.replace "'" "\\'"
