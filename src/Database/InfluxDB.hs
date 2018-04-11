{- |
stability: experimental
portability: GHC
-}
module Database.InfluxDB
  ( -- $intro

  -- * Writing data via HTTP
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
  , F.formatQuery
  , (F.%)

  -- ** Query parameters
  , QueryParams
  , queryParams
  , authentication

  -- ** Parsing results
  -- $parsing-results
  , QueryResults(..)
  , parseResultsWith
  , parseResultsWithDecoder
  , Decoder(..)
  , lenientDecoder
  , strictDecoder
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
  , F.formatDatabase
  , Measurement
  , F.formatMeasurement
  , Key
  , F.formatKey

  , Server
  , defaultServer
  , host
  , port
  , ssl

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

import Database.InfluxDB.JSON
import Database.InfluxDB.Line
import Database.InfluxDB.Manage (manage)
import Database.InfluxDB.Query
import Database.InfluxDB.Types
import Database.InfluxDB.Write
import qualified Database.InfluxDB.Format as F

{- $intro
= Getting started

This tutorial assumes the following language extensions and imports.

>>> :set -XOverloadedStrings
>>> :set -XRecordWildCards
>>> import Database.InfluxDB
>>> import qualified Database.InfluxDB.Format as F
>>> import Control.Lens
>>> import qualified Data.Map as Map
>>> import Data.Time
>>> import qualified Data.Vector as V

The examples below roughly follows the
 [README](https://github.com/influxdata/influxdb/blob/0b4528b26de43d5504ec0623c184540f7c3e1a54/client/README.md)
in the official Go client library.

== Creating a database

This library assumes the [lens](https://hackage.haskell.org/package/lens)
package in some APIs. Here we use 'Control.Lens.?~' to set the authentication
parameters of type @Maybe 'Credentials'@.

Also note that in order to construct a 'Query', we use 'F.formatQuery' with the
'F.database' formatter. There are many other formatters defined in
"Database.InfluxDB.Format".

>>> let db = "square_holes"
>>> let bubba = credentials "bubba" "bumblebeetuna"
>>> let p = queryParams db & authentication ?~ bubba
>>> manage p $ formatQuery ("DROP DATABASE "%F.database) db
>>> manage p $ formatQuery ("CREATE DATABASE "%F.database) db

== Writing data

'write' or 'writeBatch' can be used to write data. In general 'writeBatch'
should be used for efficiency when writing multiple data points.

>>> let wp = writeParams db & authentication ?~ bubba & precision .~ Second
>>> let cpuUsage = "cpu_usage"
>>> :{
writeBatch wp
  [ Line cpuUsage (Map.singleton "cpu" "cpu-total")
    (Map.fromList
      [ ("idle",   FieldFloat 10.1)
      , ("system", FieldFloat 53.3)
      , ("user",   FieldFloat 46.6)
      ])
    (Just $ parseTimeOrError False defaultTimeLocale
      "%F %T%Q %Z"
      "2017-06-17 15:41:40.42659044 UTC") :: Line UTCTime
  ]
:}

Note that the type signature of the timestamp is necessary. Otherwise it doesn't
type check.

== Querying data

First we define a placeholder data type called 'CPUUsage' and a 'QueryResults'
instance. 'getField', 'parseUTCTime' and 'parseQueryField' etc are avilable to
make JSON decoding easier.

>>> :{
data CPUUsage = CPUUsage
  { time :: UTCTime
  , cpuIdle, cpuSystem, cpuUser :: Double
  } deriving Show
instance QueryResults CPUUsage where
  parseResults prec = parseResultsWithDecoder strictDecoder $ \_ _ columns fields -> do
    time <- getField "time" columns fields >>= parseUTCTime prec
    FieldFloat cpuIdle <- getField "idle" columns fields >>= parseQueryField
    FieldFloat cpuSystem <- getField "system" columns fields >>= parseQueryField
    FieldFloat cpuUser <- getField "user" columns fields >>= parseQueryField
    return CPUUsage {..}
:}

>>> query p $ formatQuery ("SELECT * FROM "%F.measurement) cpuUsage :: IO (V.Vector CPUUsage)
[CPUUsage {time = 2017-06-17 15:41:40 UTC, cpuIdle = 10.1, cpuSystem = 53.3, cpuUser = 46.6}]

Note that the type signature on query here is also necessary to type check.
-}

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
