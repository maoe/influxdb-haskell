{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.InfluxDB.Ping
  (
  -- * Ping interface
    ping

  -- * Ping parameters
  , PingParams(..)
  , pingParams
  , Types.server
  , Types.manager
  , waitForLeader

  -- * Ping result
  , PingResult(..)
  , roundtripTime
  , influxdbVersion
  ) where

import Control.Lens
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Client as HC
import System.Clock

import Database.InfluxDB.Types as Types


-- Ping requests do not require authentication
-- | The full set of parameters for the ping API
data PingParams = PingParams
  { _server :: !Server
  , _manager :: !(Either HC.ManagerSettings HC.Manager)
  -- ^ HTTP connection manager
  , _waitForLeader :: !(Maybe Int)
  -- ^ the number of seconds to wait
  }

makeLensesWith (lensRules & generateSignatures .~ False) ''PingParams

server :: Lens' PingParams Server

instance HasServer PingParams where
  server = Database.InfluxDB.Ping.server

manager :: Lens' PingParams (Either HC.ManagerSettings HC.Manager)

instance HasManager PingParams where
  manager = Database.InfluxDB.Ping.manager

-- | The number of seconds to wait before returning a response
waitForLeader :: Lens' PingParams (Maybe Int)

pingParams :: PingParams
pingParams =
  PingParams
  { _server = localServer
  , _manager = Left HC.defaultManagerSettings
  , _waitForLeader = Nothing
  }

pingRequest :: PingParams -> HC.Request
pingRequest PingParams {..} = HC.defaultRequest
  { HC.host = TE.encodeUtf8 _host
  , HC.port = fromIntegral _port
  , HC.secure = _ssl
  , HC.method = "GET"
  , HC.path = "/ping"
  }
  where
    Server {..} = _server

data PingResult = PingResult
  { _roundtripTime :: !TimeSpec
  , _influxdbVersion :: !BS.ByteString
  } deriving (Show, Eq, Ord)

makeLensesWith (lensRules & generateSignatures .~ False) ''PingResult

-- | Roundtrip time of the ping
roundtripTime :: Lens' PingResult TimeSpec

-- | Version string returned by the InfluxDB header
influxdbVersion :: Lens' PingResult BS.ByteString

ping :: PingParams -> IO PingResult
ping params = do
  manager' <- either HC.newManager return $ _manager params
  startTime <- getTime'
  HC.withResponse request manager' (\response -> do
    endTime <- getTime'
    let headers = HC.responseHeaders response
    case lookup "X-Influxdb-Version" headers of
      Just version -> pure (PingResult (diffTimeSpec endTime startTime) version)
      Nothing -> error "A response by influxdb should always contain a version header.")
  where
    request = pingRequest params
    getTime' = getTime Monotonic
