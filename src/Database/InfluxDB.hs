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

  -- * HTTP API
  -- ** Data types
  , Config(..)
  , Credentials(..), rootCreds
  , TimePrecision(..)
  , Server(..), localServer
  , ServerPool, newServerPool
  , Database(..)
  , User(..)
  , Admin(..)

  -- ** Writing Data

  -- *** Updating Points
  , post, postWithPrecision
  , SeriesT, PointT
  , writeSeries
  , withSeries
  , writePoints

  -- *** Deleting Points
  -- **** One Time Deletes (not implemented)
  -- , deleteSeries
  -- **** Regularly Scheduled Deletes (not implemented)
  -- , getScheduledDeletes
  -- , addScheduledDelete
  -- , removeScheduledDelete

  -- ** Querying Data
  , query
  , Stream(..)
  , queryChunked

  -- ** Administration & Security
  -- *** Creating and Dropping Databases
  , listDatabases
  , createDatabase
  , dropDatabase

  -- *** Security
  -- **** Cluster admin
  , listClusterAdmins
  , addClusterAdmin
  , updateClusterAdminPassword
  , deleteClusterAdmin
  -- **** Database user
  , listDatabaseUsers
  , addDatabaseUser
  , updateDatabaseUserPassword
  , deleteDatabaseUser
  , grantAdminPrivilegeTo
  , revokeAdminPrivilegeFrom
  ) where

import Database.InfluxDB.Encode
import Database.InfluxDB.Http
import Database.InfluxDB.Types
