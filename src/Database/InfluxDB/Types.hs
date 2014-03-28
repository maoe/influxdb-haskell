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

import Control.Applicative (empty)
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

import Data.Aeson ((.=), (.:))
import Data.Aeson.TH
import Data.Scientific
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

-- | A series consists of name, columns and points. The columns and points are
-- expressed in a separate type @SeriesData@.
data Series = Series
  { seriesName :: {-# UNPACK #-} !Text
  , seriesData :: {-# UNPACK #-} !SeriesData
  }

-- | Convenient accessor for columns.
seriesColumns :: Series -> Vector Column
seriesColumns = seriesDataColumns . seriesData

-- | Convenient accessor for points.
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

instance A.FromJSON Series where
  parseJSON (A.Object v) = do
    name <- v .: "name"
    columns <- v .: "columns"
    points <- v .: "points"
    return Series
      { seriesName = name
      , seriesData = SeriesData
          { seriesDataColumns = columns
          , seriesDataPoints = DL.fromList points
          }
      }
  parseJSON _ = empty

-- | @SeriesData@ consists of columns and points.
data SeriesData = SeriesData
  { seriesDataColumns :: Vector Column
  , seriesDataPoints :: DList (Vector Value)
  }

type Column = Text

-- | An InfluxDB value represented as a Haskell value.
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

instance A.FromJSON Value where
  parseJSON (A.Object o) = fail $ "Unexpected object: " ++ show o
  parseJSON (A.Array a) = fail $ "Unexpected array: " ++ show a
  parseJSON (A.String xs) = return $ String xs
  parseJSON (A.Bool b) = return $ Bool b
  parseJSON A.Null = return Null
  parseJSON (A.Number n) = return $! if base10Exponent n == 0
    then Int $ fromIntegral $ coefficient n
    else Float $ realToFrac n

-----------------------------------------------------------

-- | User credentials.
data Credentials = Credentials
  { credsUser :: !Text
  , credsPassword :: !Text
  } deriving Show

-- | Server location.
data Server = Server
  { serverHost :: !Text
  , serverPort :: !Int
  , serverSsl :: !Bool
  } deriving Show

-- | Non-empty set of server locations. The active server will always be used
-- until any HTTP communications fail.
data ServerPool = ServerPool
  { serverActive :: !Server
  , serverBackup :: !(Seq Server)
  }

-- | Database consits of name and replication factor.
data Database = Database
  { databaseName :: !Text
  , databaseReplicationFactor :: !(Maybe Int)
  } deriving Show

newtype ScheduledDelete = ScheduledDelete
  { scheduledDeleteId :: Int
  } deriving Show

-- | User
newtype User = User
  { userName :: Text
  } deriving Show

-- | Administrator
newtype Admin = Admin
  { adminUsername :: Text
  } deriving Show


-----------------------------------------------------------
-- Server pool manipulation

-- | Create a non-empty server pool. You must specify at least one server
-- location to create a pool.
newServerPool :: Server -> [Server] -> IO (IORef ServerPool)
newServerPool active backups = newIORef ServerPool
  { serverActive = active
  , serverBackup = Seq.fromList backups
  }

-- | Get a server from the pool.
activeServer :: IORef ServerPool -> IO Server
activeServer ref = do
  ServerPool { serverActive } <- readIORef ref
  return serverActive

-- | Move the current server to the backup pool and pick one of the backup
-- server as the new active server. Currently the scheduler works in
-- round-robin fashion.
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
