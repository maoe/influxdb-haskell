module Database.InfluxDB
  (
  -- * Series data types
    Series(..), seriesColumns, seriesPoints
  , SeriesData(..)
  , Value(..)

  -- ** Encoding
  , ToSeriesData(..)
  , ToValue(..)

  -- ** Decoding
  , FromSeries(..), fromSeries
  , FromSeriesData(..), fromSeriesData
  , FromValue(..), fromValue

  , withValues, (.:), (.:?), (.!=)
  , typeMismatch

  -- * HTTP API
  -- ** Data types
  , Config(..)
  , Credentials(..), rootCreds
  , TimePrecision(..)
  , Server(..), localServer
  , ServerPool, newServerPool
  , newServerPoolWithRetryPolicy, newServerPoolWithRetrySettings
  , Database(..)
  , User(..)
  , Admin(..)
  , Ping(..)
  , ShardSpace(..)

  -- *** Exception
  , InfluxException(..)

  -- ** Writing Data

  -- *** Updating Points
  , post, postWithPrecision
  , SeriesT, PointT
  , writeSeries
  , writeSeriesData
  , withSeries
  , writePoints

  -- *** Deleting Points
  , deleteSeries

  -- ** Querying Data
  , query
  , Stream(..)
  , queryChunked

  -- ** Administration & Security
  -- *** Creating and Dropping Databases
  , listDatabases
  , createDatabase
  , dropDatabase

  , DatabaseRequest(..)
  , configureDatabase

  -- *** Security
  -- **** Shard spaces
  , ShardSpaceRequest(..)
  , listShardSpaces
  , createShardSpace
  , dropShardSpace

  -- **** Cluster admin
  , listClusterAdmins
  , authenticateClusterAdmin
  , addClusterAdmin
  , updateClusterAdminPassword
  , deleteClusterAdmin
  -- **** Database user
  , listDatabaseUsers
  , authenticateDatabaseUser
  , addDatabaseUser
  , updateDatabaseUserPassword
  , deleteDatabaseUser
  , grantAdminPrivilegeTo
  , revokeAdminPrivilegeFrom

  -- *** Other API
  , ping
  , isInSync
  ) where

import Database.InfluxDB.Decode
import Database.InfluxDB.Encode
import Database.InfluxDB.Http
import Database.InfluxDB.Types
