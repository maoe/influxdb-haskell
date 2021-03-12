{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-missing-signatures #-}
#else
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
#endif
module Database.InfluxDB.Ping
  ( -- * Ping interface
    ping

  -- * Ping parameters
  , PingParams
  , pingParams
  , server
  , manager
  , timeout

  -- * Pong
  , Pong
  , roundtripTime
  , influxdbVersion
  ) where
import Control.Exception

import Control.Lens
import Data.Time.Clock (NominalDiffTime)
import System.Clock
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Client as HC

import Database.InfluxDB.Types as Types

-- $setup
-- >>> import Database.InfluxDB.Ping

-- Ping requests do not require authentication
-- | The full set of parameters for the ping API
--
-- Following lenses are available to access its fields:
--
-- * 'server'
-- * 'manager'
-- * 'timeout'
data PingParams = PingParams
  { pingServer :: !Server
  , pingManager :: !(Either HC.ManagerSettings HC.Manager)
  -- ^ HTTP connection manager
  , pingTimeout :: !(Maybe NominalDiffTime)
  -- ^ Timeout
  }

-- | Smart constructor for 'PingParams'
--
-- Default parameters:
--
--   ['server'] 'defaultServer'
--   ['manager'] @'Left' 'HC.defaultManagerSettings'@
--   ['timeout'] 'Nothing'
pingParams :: PingParams
pingParams = PingParams
  { pingServer = defaultServer
  , pingManager = Left HC.defaultManagerSettings
  , pingTimeout = Nothing
  }

makeLensesWith
  ( lensRules
    & generateSignatures .~ False
    & lensField .~ lookingupNamer
      [ ("pingServer", "_server")
      , ("pingManager", "_manager")
      , ("pingTimeout", "timeout")
      ]
    )
  ''PingParams

-- |
-- >>> pingParams ^. server.host
-- "localhost"
instance HasServer PingParams where
  server = _server

-- |
-- >>> let p = pingParams & manager .~ Left HC.defaultManagerSettings
instance HasManager PingParams where
  manager = _manager

-- | The number of seconds to wait before returning a response
--
-- >>> pingParams ^. timeout
-- Nothing
-- >>> let p = pingParams & timeout ?~ 1
timeout :: Lens' PingParams (Maybe NominalDiffTime)

pingRequest :: PingParams -> HC.Request
pingRequest PingParams {..} = HC.defaultRequest
  { HC.host = TE.encodeUtf8 _host
  , HC.port = fromIntegral _port
  , HC.secure = _ssl
  , HC.method = "GET"
  , HC.path = "/ping"
  }
  where
    Server {..} = pingServer

-- | Response of a ping request
data Pong = Pong
  { _roundtripTime :: !TimeSpec
  -- ^ Round-trip time of the ping
  , _influxdbVersion :: !BS.ByteString
  -- ^ Version string returned by InfluxDB
  } deriving (Show, Eq, Ord)

makeLensesWith (lensRules & generateSignatures .~ False) ''Pong

-- | Round-trip time of the ping
roundtripTime :: Lens' Pong TimeSpec

-- | Version string returned by InfluxDB
influxdbVersion :: Lens' Pong BS.ByteString

-- | Send a ping to InfluxDB.
--
-- It may throw an 'InfluxException'.
ping :: PingParams -> IO Pong
ping params = do
  manager' <- either HC.newManager return $ pingManager params
  startTime <- getTimeMonotonic
  HC.withResponse request manager' $ \response -> do
    endTime <- getTimeMonotonic
    case lookup "X-Influxdb-Version" (HC.responseHeaders response) of
      Just version ->
        return $! Pong (diffTimeSpec endTime startTime) version
      Nothing ->
        throwIO $ UnexpectedResponse
          "The X-Influxdb-Version header was missing in the response."
          request
          ""
  `catch` (throwIO . HTTPException)
  where
    request = (pingRequest params)
      { HC.responseTimeout = case pingTimeout params of
        Nothing -> HC.responseTimeoutNone
        Just sec -> HC.responseTimeoutMicro $
          round $ realToFrac sec / (10**(-6) :: Double)
      }
    getTimeMonotonic = getTime Monotonic
