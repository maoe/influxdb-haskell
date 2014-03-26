{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
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
  , ServerPool(..)
  , Database(..)
  , ScheduledDelete(..)
  , User(..)
  , Admin(..)

  , newServerPool
  , activeServer
  , failover
  ) where

import Data.DList (DList)
import Data.Data (Data)
import Data.IORef
import Data.Int (Int64)
import Data.Sequence (Seq, ViewL(..), (|>))
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import qualified Data.DList as DL
import qualified Data.Sequence as Seq

import Data.Aeson ((.=))
import Data.Aeson.TH
import qualified Data.Aeson as A

import Database.InfluxDB.Types.Internal (stripPrefixOptions)

-----------------------------------------------------------
-- Compatibility for older GHC

#if __GLASGOW_HASKELL__ < 706
import Control.Exception (evaluate)

atomicModifyIORef' :: IORef a -> (a -> (a, b)) -> IO b
atomicModifyIORef' ref f = do
    b <- atomicModifyIORef ref $ \x ->
      let (a, b) = f x
      in (a, a `seq` b)
    evaluate b
#endif
-----------------------------------------------------------

data Series = Series
  { seriesName :: {-# UNPACK #-} !Text
  , seriesData :: {-# UNPACK #-} !SeriesData
  }

seriesColumns :: Series -> Vector Column
seriesColumns = seriesDataColumns . seriesData

seriesPoints :: Series -> DList (Vector Value)
seriesPoints = seriesDataPoints . seriesData

instance A.ToJSON Series where
  toJSON Series {..} = A.object
    [ "name" .= seriesName
    , "columns" .= seriesDataColumns
    , "points" .= DL.toList seriesDataPoints
    ]
    where
      SeriesData {..} = seriesData

data SeriesData = SeriesData
  { seriesDataColumns :: Vector Column
  , seriesDataPoints :: DList (Vector Value)
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
  , serverSsl :: !Bool
  } deriving Show

data ServerPool = ServerPool
  { serverActive :: !Server
  , serverBackup :: !(Seq Server)
  }

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
-- Server pool manipulation

newServerPool :: Server -> [Server] -> IO (IORef ServerPool)
newServerPool active backups = newIORef ServerPool
  { serverActive = active
  , serverBackup = Seq.fromList backups
  }

activeServer :: IORef ServerPool -> IO Server
activeServer ref = do
  ServerPool { serverActive } <- readIORef ref
  return serverActive

failover :: IORef ServerPool -> IO ()
failover ref = atomicModifyIORef' ref $ \pool@ServerPool {..} ->
  case Seq.viewl serverBackup of
    EmptyL -> (pool, ())
    active :< rest -> (pool', ())
      where
        pool' = ServerPool
          { serverActive = active
          , serverBackup = rest |> serverActive
          }

-----------------------------------------------------------
-- Aeson instances

deriveFromJSON (stripPrefixOptions "database") ''Database
deriveFromJSON (stripPrefixOptions "admin") ''Admin
deriveFromJSON (stripPrefixOptions "user") ''User
