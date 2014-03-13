{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Database.InfluxDB.Types
  ( Series(..)
  , seriesColumns
  , seriesPoints
  , SeriesData(..)
  , Column
  , Value(..)

  , Credentials(..)
  , Server(..)
  , Database(..)
  , ScheduledDelete(..)
  , User(..)
  , Admin(..)
  ) where

import Data.Data (Data)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Typeable (Typeable)

import Data.Aeson ((.=))
import Data.Aeson.TH
import Data.Vector (Vector)
import qualified Data.Aeson as A

import Database.InfluxDB.Types.Internal (stripPrefixOptions)

data Series = Series
  { seriesName :: {-# UNPACK #-} !Text
  , seriesData :: {-# UNPACK #-} !SeriesData
  }

seriesColumns :: Series -> Vector Column
seriesColumns = seriesDataColumns . seriesData

seriesPoints :: Series -> [Vector Value]
seriesPoints = seriesDataPoints . seriesData

instance A.ToJSON Series where
  toJSON Series {..} = A.object
    [ "name" .= seriesName
    , "columns" .= seriesDataColumns
    , "points" .= seriesDataPoints
    ]
    where
      SeriesData {..} = seriesData

data SeriesData = SeriesData
  { seriesDataColumns :: !(Vector Column)
  , seriesDataPoints :: [Vector Value]
  }

type Column = Text

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

-----------------------------------------------------------

data Credentials = Credentials
  { credsUser :: !Text
  , credsPassword :: !Text
  } deriving Show

data Server = Server
  { serverHost :: !Text
  , serverPort :: !Int
  } deriving Show

data Database = Database
  { databaseName :: !Text
  , databaseReplicationFactor :: !(Maybe Int)
  } deriving Show

newtype ScheduledDelete = ScheduledDelete
  { scheduledDeleteId :: Int
  } deriving Show

newtype User = User
  { userName :: Text
  } deriving Show

newtype Admin = Admin
  { adminUsername :: Text
  } deriving Show


-----------------------------------------------------------
-- Aeson instances

deriveFromJSON (stripPrefixOptions "database") ''Database
deriveFromJSON (stripPrefixOptions "admin") ''Admin
deriveFromJSON (stripPrefixOptions "user") ''User
