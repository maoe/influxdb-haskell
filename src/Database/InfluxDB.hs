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

  , Field(..)
  , LineField
  , QueryField
  , Timestamp(..)
  , precisionScale
  , precisionName

  -- * Querying data
  , Query
  , query
  , queryChunked

  -- * Query constructor
  , formatQuery
  , (%)

  -- ** Query parameters
  , QueryParams
  , queryParams
  , authentication

  -- ** Parsing results
  , QueryResults(..)
  , parseResultsWith
  , getField
  , getTag
  , parseTimestamp
  , parseFieldValue
  , parseKey

  -- * Database management
  , manage

  -- * Common data types and classes
  , Precision(..)
  , Database
  , Key

  , Server
  , host
  , port
  , ssl
  , localServer

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

import Database.InfluxDB.Format ((%), formatQuery)
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
