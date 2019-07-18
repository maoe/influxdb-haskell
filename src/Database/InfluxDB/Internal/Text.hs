{-# LANGUAGE OverloadedStrings #-}
module Database.InfluxDB.Internal.Text
  ( escapeCommas
  , escapeEqualSigns
  , escapeSpaces
  , escapeDoubleQuotes
  , escapeSingleQuotes
  , escapeBackslashes
  ) where
import Data.Text (Text)
import qualified Data.Text as T

escapeCommas
  , escapeEqualSigns
  , escapeSpaces
  , escapeDoubleQuotes
  , escapeSingleQuotes
  , escapeBackslashes :: Text -> Text
escapeCommas = T.replace "," "\\,"
escapeEqualSigns = T.replace "=" "\\="
escapeSpaces = T.replace " " "\\ "
escapeDoubleQuotes = T.replace "\"" "\\\""
escapeSingleQuotes = T.replace "'" "\\'"
escapeBackslashes = T.replace "\\" "\\\\"
