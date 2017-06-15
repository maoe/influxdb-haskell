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
  -- $query
  , Query
  , query
  , queryChunked

  -- ** Query construction
  -- $query-construction
  , formatQuery
  , (%)

  -- ** Query parameters
  , QueryParams
  , queryParams
  , authentication

  -- ** Parsing results
  -- $parsing-results
  , QueryResults(..)
  , parseResultsWith
  , getField
  , getTag
  , parseUTCTime
  , parsePOSIXTime
  , parseQueryField

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
  , defaultServer

  , Credentials
  , credentials
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

{- $query
'query' and 'queryChunked' can be used to query data. If your dataset fits your
memory, 'query' is easier to use. If it doesn't, use 'queryChunked' to stream
data.
-}

{- $query-construction
There are various utility functions available in "Database.InfluxDB.Format".
This module is designed to be imported as qualified:

@
import "Database.InfluxDB"
import qualified "Database.InfluxDB.Format" as F
@
-}
