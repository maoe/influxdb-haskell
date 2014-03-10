{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Database.InfluxDB.Types
  ( Series(..)
  , seriesColumns
  , seriesPoints
  , SeriesData(..)
  , Column
  , Value(..)
  ) where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Data (Data)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Typeable (Typeable)
import qualified Data.Text.Encoding as TE

import Data.Aeson ((.=))
import Data.Vector (Vector)
import qualified Data.Aeson as A

data Series = Series
  { seriesName :: {-# UNPACK #-} !ByteString
  , seriesData :: {-# UNPACK #-} !SeriesData
  }

seriesColumns :: Series -> Vector Column
seriesColumns = seriesDataColumns . seriesData

seriesPoints :: Series -> [Vector Value]
seriesPoints = seriesDataPoints . seriesData

instance A.ToJSON Series where
  toJSON Series {..} = A.object
    [ "name" .= TE.decodeUtf8 seriesName
    , "columns" .= (TE.decodeUtf8 <$> seriesDataColumns)
    , "points" .= seriesDataPoints
    ]
    where
      SeriesData {..} = seriesData

data SeriesData = SeriesData
  { seriesDataColumns :: !(Vector Column)
  , seriesDataPoints :: [Vector Value]
  }

type Column = ByteString

data Value
  = Int !Int64
  | Float !Double
  | String !Text
  | Bool !Bool
  | Null
  deriving (Eq, Show, Data, Typeable)

instance A.ToJSON Value where
  toJSON (Int n) = A.toJSON n
  toJSON (Float d) = A.toJSON d
  toJSON (String xs) = A.toJSON xs
  toJSON (Bool b) = A.toJSON b
  toJSON Null = A.Null
