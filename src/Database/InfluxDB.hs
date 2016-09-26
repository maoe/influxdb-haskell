{- |
stability: experimental
portability: GHC
-}
module Database.InfluxDB
  ( -- $intro

  -- * Writing data
  -- $write
    write
  , writeBatch
  , writeByteString

  -- ** Write parameters
  , WriteParams
  , writeParams
  , retentionPolicy

  -- ** The Line protocol
  , Line(Line)
  , measurement
  , tagSet
  , fieldSet
  , timestamp

  , FieldValue(..)
  , Timestamp(..)
  , precisionScale

  -- * Querying data
  , Query
  , query
  , queryChunked

  -- ** Query parameters
  , QueryParams
  , queryParams
  , authentication

  -- ** Parsing results
  , QueryResults(..)
  , parseResultsWith
  , parseTimestamp
  , parseKey

  -- * Database management
  , manage

  -- * Common data types and classes
  , Precision(..)
  , RequestType
  , Database
  , RetentionPolicy
  , Key

  , Server
  , host
  , port
  , ssl

  , Credentials
  , user
  , password

  -- * Exception
  , InfluxException(..)

  , HasServer(..)
  , HasDatabase(..)
  , HasPrecision(..)
  , HasManager(..)
  ) where

import Database.InfluxDB.JSON
import Database.InfluxDB.Line
import Database.InfluxDB.Manage (manage)
import Database.InfluxDB.Query
import Database.InfluxDB.Types
import Database.InfluxDB.Write

{- $write
InfluxDB has two ways to write data into it, via HTTP and UDP. This module
only exports functions for the HTTP API. For UDP, you can use a qualified
import:

@
import qualified "Database.InfluxDB.Write.UDP" as UDP
@
-}
