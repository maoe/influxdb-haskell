{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Database.InfluxDB.Write.UDP
  ( -- $intro

  -- * Writers
    write
  , writeBatch
  , writeByteString

  -- * Writer parameters
  , WriteParams
  , writeParams
  , socket
  , sockAddr
  , Types.precision
  ) where

import Control.Lens
import Network.Socket (SockAddr, Socket)
import Network.Socket.ByteString (sendManyTo)
import qualified Data.ByteString.Lazy as BL

import Database.InfluxDB.Line
import Database.InfluxDB.Types as Types

{- $intro
This module is desined to be used with the [network]
(https://hackage.haskell.org/package/network) package and be imported qualified.

>>> :set -XOverloadedStrings -XNoOverloadedLists
>>> import qualified Data.Map as Map
>>> import Data.Time
>>> import Network.Socket
>>> import Database.InfluxDB
>>> import qualified Database.InfluxDB.Write.UDP as UDP
>>> sock <- Network.Socket.socket AF_INET Datagram defaultProtocol
>>> let localhost = tupleToHostAddress (127, 0, 0, 1)
>>> let params = UDP.writeParams sock $ SockAddrInet 8089 localhost
>>> UDP.write params $ Line "measurement1" Map.empty (Map.fromList [("value", FieldInt 42)]) (Nothing :: Maybe UTCTime)
>>> close sock

Make sure that the UDP service is enabled in the InfluxDB config. This API
doesn't tell you if any error occurs. See [the official doc]
(https://docs.influxdata.com/influxdb/v1.6/supported_protocols/udp/) for
details.
-}

-- | The full set of parameters for the UDP writer.
data WriteParams = WriteParams
  { _socket :: !Socket
  , _sockAddr :: !SockAddr
  , _precision :: !(Precision 'WriteRequest)
  }

-- | Smart constructor for 'WriteParams'
--
-- Default parameters:
--
--   ['L.precision'] 'Nanosecond'
writeParams :: Socket -> SockAddr -> WriteParams
writeParams _socket _sockAddr = WriteParams
  { _precision = Nanosecond
  , ..
  }

-- | Write a 'Line'
write
  :: Timestamp time
  => WriteParams
  -> Line time
  -> IO ()
write p@WriteParams {_precision} =
  writeByteString p . encodeLine (roundTo _precision)

-- | Write 'Line's in a batch
--
-- This is more efficient than 'write'.
writeBatch
  :: (Timestamp time, Foldable f)
  => WriteParams
  -> f (Line time)
  -> IO ()
writeBatch p@WriteParams {_precision} =
  writeByteString p . encodeLines (roundTo _precision)

-- | Write a lazy 'L.ByteString'
writeByteString :: WriteParams -> BL.ByteString -> IO ()
writeByteString WriteParams {..} payload =
  sendManyTo _socket (BL.toChunks payload) _sockAddr

makeLensesWith (lensRules & generateSignatures .~ False) ''WriteParams

-- | Open UDP socket
socket :: Lens' WriteParams Socket

-- | UDP endopoint of the database
sockAddr :: Lens' WriteParams SockAddr

precision :: Lens' WriteParams (Precision 'WriteRequest)

-- | Timestamp precision.
--
-- In the UDP API, all timestamps are sent in nanosecond but you can specify
-- lower precision. The writer just rounds timestamps to the specified
-- precision.
instance HasPrecision 'WriteRequest WriteParams where
  precision = Database.InfluxDB.Write.UDP.precision
