{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Database.InfluxDB.Types where
import Control.Exception
import Data.Int (Int64)
import Data.String
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Control.Lens
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Network.HTTP.Client (Manager, ManagerSettings, Request)
import System.Clock (TimeSpec(..))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Network.HTTP.Client as HC

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import System.Clock (TimeSpec(..))
-- >>> import Database.InfluxDB
-- >>> import qualified Database.InfluxDB.Format as F

-- | An InfluxDB query.
--
-- A spec of the format is available at
-- <https://docs.influxdata.com/influxdb/v1.7/query_language/spec/>.
--
-- A 'Query' can be constructed using either
--
--   * the 'IsString' instance with @-XOverloadedStrings@
--   * or 'Database.InfluxDB.Format.formatQuery'.
--
-- >>> :set -XOverloadedStrings
-- >>> "SELECT * FROM series" :: Query
-- "SELECT * FROM series"
-- >>> import qualified Database.InfluxDB.Format as F
-- >>> formatQuery ("SELECT * FROM "%F.key) "series"
-- "SELECT * FROM \"series\""
--
-- NOTE: Currently this library doesn't support type-safe query construction.
newtype Query = Query T.Text deriving IsString

instance Show Query where
  show (Query q) = show q

-- | InfluxDB server to connect to.
--
-- Following lenses are available to access its fields:
--
-- * 'host': FQDN or IP address of the InfluxDB server
-- * 'port': Port number of the InfluxDB server
-- * 'ssl': Whether or not to use SSL
data Server = Server
  { _host :: !Text
  , _port :: !Int
  , _ssl :: !Bool
  } deriving (Show, Generic, Eq, Ord)

makeLensesWith (lensRules & generateSignatures .~ False) ''Server

-- | Host name of the server
host :: Lens' Server Text

-- | Port number of the server
port :: Lens' Server Int

-- | If SSL is enabled
--
-- For secure connections (HTTPS), consider using one of the following packages:
--
--  * [http-client-tls](https://hackage.haskell.org/package/http-client-tls)
--  * [http-client-openssl](https://hackage.haskell.org/package/http-client-openssl)
ssl :: Lens' Server Bool

-- | Default InfluxDB server settings
--
-- Default parameters:
--
-- >>> defaultServer ^. host
-- "localhost"
-- >>> defaultServer ^. port
-- 8086
-- >>> defaultServer ^. ssl
-- False
defaultServer :: Server
defaultServer = Server
  { _host = "localhost"
  , _port = 8086
  , _ssl = False
  }

-- | HTTPS-enabled InfluxDB server settings
secureServer :: Server
secureServer = defaultServer & ssl .~ True

-- | User credentials.
--
-- Following lenses are available to access its fields:
--
-- * 'user'
-- * 'password'
data Credentials = Credentials
  { _user :: !Text
  , _password :: !Text
  } deriving Show

-- | Smart constructor for 'Credentials'
credentials
    :: Text -- ^ User name
    -> Text -- ^ Password
    -> Credentials
credentials = Credentials

makeLensesWith (lensRules & generateSignatures .~ False) ''Credentials

-- | User name to access InfluxDB.
--
-- >>> let creds = credentials "john" "passw0rd"
-- >>> creds ^. user
-- "john"
user :: Lens' Credentials Text

-- | Password to access InfluxDB
--
-- >>> let creds = credentials "john" "passw0rd"
-- >>> creds ^. password
-- "passw0rd"
password :: Lens' Credentials Text

-- | Database name.
--
-- 'Database.InfluxDB.formatDatabase' can be used to construct a
-- 'Database'.
--
-- >>> "test-db" :: Database
-- "test-db"
-- >>> formatDatabase "test-db"
-- "test-db"
-- >>> formatDatabase ("test-db-"%F.decimal) 0
-- "test-db-0"
newtype Database = Database { databaseName :: Text } deriving (Eq, Ord)

instance IsString Database where
  fromString xs = Database $ identifier "Database" xs

instance Show Database where
  show (Database name) = show name

-- | String name that is used for measurements.
--
-- 'Database.InfluxDB.formatMeasurement' can be used to construct a
-- 'Measurement'.
--
-- >>> "test-series" :: Measurement
-- "test-series"
-- >>> formatMeasurement "test-series"
-- "test-series"
-- >>> formatMeasurement ("test-series-"%F.decimal) 0
-- "test-series-0"
newtype Measurement = Measurement Text deriving (Eq, Ord)

instance IsString Measurement where
  fromString xs = Measurement $ identifier "Measurement" xs

instance Show Measurement where
  show (Measurement name) = show name

-- | String type that is used for tag keys/values and field keys.
--
-- 'Database.InfluxDB.formatKey' can be used to construct a 'Key'.
--
-- >>> "test-key" :: Key
-- "test-key"
-- >>> formatKey "test-key"
-- "test-key"
-- >>> formatKey ("test-key-"%F.decimal) 0
-- "test-key-0"
newtype Key = Key Text deriving (Eq, Ord)

instance IsString Key where
  fromString xs = Key $ identifier "Key" xs

instance Show Key where
  show (Key name) = show name

identifier :: String -> String -> Text
identifier ty xs
  | null xs = error $ ty ++ " should never be empty"
  | elem '\n' xs = error $ ty ++ " should not contain a new line"
  | otherwise = fromString xs

-- | Nullability of fields.
--
-- Queries can contain nulls but the line protocol cannot.
data Nullability = Nullable | NonNullable deriving Typeable

-- | Field type for queries. Queries can contain null values.
type QueryField = Field 'Nullable

-- | Field type for the line protocol. The line protocol doesn't accept null
-- values.
type LineField = Field 'NonNullable

data Field (n :: Nullability) where
  -- | Signed 64-bit integers (@-9,223,372,036,854,775,808@ to
  -- @9,223,372,036,854,775,807@).
  FieldInt :: !Int64 -> Field n
  -- | IEEE-754 64-bit floating-point numbers. This is the default numerical
  -- type.
  FieldFloat :: !Double -> Field n
  -- | String field. Its length is limited to 64KB, which is not enforced by
  -- this library.
  FieldString :: !Text -> Field n
  -- | Boolean field.
  FieldBool :: !Bool -> Field n
  -- | Null field.
  --
  -- Note that a field can be null only in queries. The line protocol doesn't
  -- allow null values.
  FieldNull :: Field 'Nullable
  deriving Typeable

deriving instance Eq (Field n)
deriving instance Show (Field n)

instance IsString (Field n) where
  fromString = FieldString . T.pack

-- | Type of a request
data RequestType
  = QueryRequest
  -- ^ Request for @/query@
  | WriteRequest
  -- ^ Request for @/write@
  deriving Show

-- | Predefined set of time precision.
--
-- 'RFC3339' is only available for 'QueryRequest's.
data Precision (ty :: RequestType) where
  -- | POSIX time in ns
  Nanosecond :: Precision ty
  -- | POSIX time in μs
  Microsecond :: Precision ty
  -- | POSIX time in ms
  Millisecond :: Precision ty
  -- | POSIX time in s
  Second :: Precision ty
  -- | POSIX time in minutes
  Minute :: Precision ty
  -- | POSIX time in hours
  Hour :: Precision ty
  -- | Nanosecond precision time in a human readable format, like
  -- @2016-01-04T00:00:23.135623Z@. This is the default format for @/query@.
  RFC3339 :: Precision 'QueryRequest

deriving instance Show (Precision a)
deriving instance Eq (Precision a)

-- | Name of the time precision.
--
-- >>> precisionName Nanosecond
-- "n"
-- >>> precisionName Microsecond
-- "u"
-- >>> precisionName Millisecond
-- "ms"
-- >>> precisionName Second
-- "s"
-- >>> precisionName Minute
-- "m"
-- >>> precisionName Hour
-- "h"
-- >>> precisionName RFC3339
-- "rfc3339"
precisionName :: Precision ty -> Text
precisionName = \case
  Nanosecond -> "n"
  Microsecond -> "u"
  Millisecond -> "ms"
  Second -> "s"
  Minute -> "m"
  Hour -> "h"
  RFC3339 -> "rfc3339"

-- | A 'Timestamp' is something that can be converted to a valid
-- InfluxDB timestamp, which is represented as a 64-bit integer.
class Timestamp time where
  -- | Round a time to the given precision and scale it to nanoseconds
  roundTo :: Precision 'WriteRequest -> time -> Int64
  -- | Scale a time to the given precision
  scaleTo :: Precision 'WriteRequest -> time -> Int64

roundAt :: RealFrac a => a -> a -> a
roundAt scale x = fromIntegral (round (x / scale) :: Int64) * scale

-- | Scale of the type precision.
--
-- >>> precisionScale RFC3339
-- 1.0e-9
-- >>> precisionScale Microsecond
-- 1.0e-6
precisionScale :: Fractional a => Precision ty -> a
precisionScale = \case
  RFC3339 ->     10^^(-9 :: Int)
  Nanosecond ->  10^^(-9 :: Int)
  Microsecond -> 10^^(-6 :: Int)
  Millisecond -> 10^^(-3 :: Int)
  Second -> 1
  Minute -> 60
  Hour ->   60 * 60

-- |
-- >>> import Data.Time.Calendar
-- >>> let t = UTCTime (fromGregorian 2018 04 14) 123.123456789
-- >>> t
-- 2018-04-14 00:02:03.123456789 UTC
-- >>> roundTo Nanosecond t
-- 1523664123123456789
-- >>> roundTo Microsecond t
-- 1523664123123457000
-- >>> roundTo Millisecond t
-- 1523664123123000000
-- >>> roundTo Second t
-- 1523664123000000000
-- >>> roundTo Minute t
-- 1523664120000000000
-- >>> roundTo Hour t
-- 1523664000000000000
-- >>> scaleTo Nanosecond t
-- 1523664123123456789
-- >>> scaleTo Microsecond t
-- 1523664123123457
-- >>> scaleTo Millisecond t
-- 1523664123123
-- >>> scaleTo Second t
-- 1523664123
-- >>> scaleTo Minute t
-- 25394402
-- >>> scaleTo Hour t
-- 423240
instance Timestamp UTCTime where
  roundTo prec = roundTo prec . utcTimeToPOSIXSeconds
  scaleTo prec = scaleTo prec . utcTimeToPOSIXSeconds

-- |
-- >>> let dt = 123.123456789 :: NominalDiffTime
-- >>> roundTo Nanosecond dt
-- 123123456789
-- >>> roundTo Microsecond dt
-- 123123457000
-- >>> roundTo Millisecond dt
-- 123123000000
-- >>> roundTo Second dt
-- 123000000000
-- >>> roundTo Minute dt
-- 120000000000
-- >>> roundTo Hour dt
-- 0
-- >>> scaleTo Nanosecond dt
-- 123123456789
-- >>> scaleTo Microsecond dt
-- 123123457
-- >>> scaleTo Millisecond dt
-- 123123
-- >>> scaleTo Second dt
-- 123
-- >>> scaleTo Minute dt
-- 2
-- >>> scaleTo Hour dt
-- 0
instance Timestamp NominalDiffTime where
  roundTo prec time =
    round $ 10^(9 :: Int) * roundAt (precisionScale prec) time
  scaleTo prec time = round $ time / precisionScale prec

-- |
-- >>> let timespec = TimeSpec 123 123456789
-- >>> roundTo Nanosecond timespec
-- 123123456789
-- >>> roundTo Microsecond timespec
-- 123123457000
-- >>> roundTo Millisecond timespec
-- 123123000000
-- >>> roundTo Second timespec
-- 123000000000
-- >>> roundTo Minute timespec
-- 120000000000
-- >>> roundTo Hour timespec
-- 0
-- >>> scaleTo Nanosecond timespec
-- 123123456789
-- >>> scaleTo Microsecond timespec
-- 123123457
-- >>> scaleTo Millisecond timespec
-- 123123
-- >>> scaleTo Second timespec
-- 123
-- >>> scaleTo Minute timespec
-- 2
-- >>> scaleTo Hour timespec
-- 0
instance Timestamp TimeSpec where
  roundTo prec t =
    round $ 10^(9 :: Int) * roundAt (precisionScale prec) (timeSpecToSeconds t)
  scaleTo prec t = round $ timeSpecToSeconds t / precisionScale prec

timeSpecToSeconds :: TimeSpec -> Double
timeSpecToSeconds TimeSpec { sec, nsec } =
  fromIntegral sec + fromIntegral nsec * 10^^(-9 :: Int)

-- | Exceptions used in this library.
--
-- In general, the library tries to convert exceptions from the dependent
-- libraries to the following types of errors.
data InfluxException
  = ServerError String
  -- ^ Server side error.
  --
  -- You can expect to get a successful response once the issue is resolved on
  -- the server side.
  | ClientError String Request
  -- ^ Client side error.
  --
  -- You need to fix your query to get a successful response.
  | UnexpectedResponse String Request BL.ByteString
  -- ^ Received an unexpected response. The 'String' field is a message and the
  -- 'BL.ByteString' field is a possibly-empty relevant payload of the response.
  --
  -- This can happen e.g. when the response from InfluxDB is incompatible with
  -- what this library expects due to an upstream format change or when the JSON
  -- response doesn't have expected fields etc.
  | HTTPException HC.HttpException
  -- ^ HTTP communication error.
  --
  -- Typical HTTP errors (4xx and 5xx) are covered by 'ClientError' and
  -- 'ServerError'. So this exception means something unusual happened. Note
  -- that if 'HC.checkResponse' is overridden to throw an 'HC.HttpException' on
  -- an unsuccessful HTTP code, this exception is thrown instead of
  -- 'ClientError' or 'ServerError'.
  deriving (Show, Typeable)

instance Exception InfluxException

-- | Class of data types that have a server field
class HasServer a where
  -- | InfluxDB server address and port that to interact with.
  server :: Lens' a Server

-- | Class of data types that have a database field
class HasDatabase a where
  -- | Database name to work on.
  database :: Lens' a Database

-- | Class of data types that have a precision field
class HasPrecision (ty :: RequestType) a | a -> ty where
  -- | Time precision parameter.
  precision :: Lens' a (Precision ty)

-- | Class of data types that have a manager field
class HasManager a where
  -- | HTTP manager settings or a manager itself.
  --
  -- If it's set to 'ManagerSettings', the library will create a 'Manager' from
  -- the settings for you.
  manager :: Lens' a (Either ManagerSettings Manager)

-- | Class of data types that has an authentication field
class HasCredentials a where
  -- | User name and password to be used when sending requests to InfluxDB.
  authentication :: Lens' a (Maybe Credentials)
