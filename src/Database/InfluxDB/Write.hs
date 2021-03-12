{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-missing-signatures #-}
#else
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
#endif
module Database.InfluxDB.Write
  ( -- * Writers
    -- $intro
    write
  , writeBatch
  , writeByteString

  -- * Writer parameters
  , WriteParams
  , writeParams
  , Types.server
  , Types.database
  , retentionPolicy
  , Types.precision
  , Types.manager
) where
import Control.Exception
import Control.Monad
import Data.Maybe

import Control.Lens
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Types as HT

import Database.InfluxDB.Line
import Database.InfluxDB.Types as Types
import Database.InfluxDB.JSON

-- $setup
-- >>> :set -XOverloadedStrings -XNoOverloadedLists -XTypeApplications
-- >>> import qualified Data.Map as Map
-- >>> import Data.Time
-- >>> import Database.InfluxDB
-- >>> import qualified Network.HTTP.Client as HC
-- >>> Database.InfluxDB.manage (queryParams "test-db") "CREATE DATABASE \"test-db\""

{- $intro
The code snippets in this module assume the following imports.

@
import qualified Data.Map as Map
import Data.Time
@
-}

-- | The full set of parameters for the HTTP writer.
--
-- Following lenses are available to access its fields:
--
-- * 'server'
-- * 'database'
-- * 'retentionPolicy'
-- * 'precision'
-- * 'authentication'
-- * 'manager'
data WriteParams = WriteParams
  { writeServer :: !Server
  , writeDatabase :: !Database
  -- ^ Database to be written
  , writeRetentionPolicy :: !(Maybe Key)
  -- ^ 'Nothing' means the default retention policy for the database.
  , writePrecision :: !(Precision 'WriteRequest)
  -- ^ Timestamp precision
  --
  -- In the HTTP API, timestamps are scaled by the given precision.
  , writeAuthentication  :: !(Maybe Credentials)
  -- ^ No authentication by default
  , writeManager :: !(Either HC.ManagerSettings HC.Manager)
  -- ^ HTTP connection manager
  }

-- | Smart constructor for 'WriteParams'
--
-- Default parameters:
--
--   ['server'] 'defaultServer'
--   ['retentionPolicy'] 'Nothing'
--   ['precision'] 'Nanosecond'
--   ['authentication'] 'Nothing'
--   ['manager'] @'Left' 'HC.defaultManagerSettings'@
writeParams :: Database -> WriteParams
writeParams writeDatabase = WriteParams
  { writeServer = defaultServer
  , writePrecision = Nanosecond
  , writeRetentionPolicy = Nothing
  , writeAuthentication = Nothing
  , writeManager = Left HC.defaultManagerSettings
  , ..
  }

-- | Write a 'Line'.
--
-- >>> let p = writeParams "test-db"
-- >>> write p $ Line @UTCTime "room_temp" Map.empty (Map.fromList [("temp", FieldFloat 25.0)]) Nothing
write
  :: Timestamp time
  => WriteParams
  -> Line time
  -> IO ()
write p@WriteParams {writePrecision} =
  writeByteString p . encodeLine (scaleTo writePrecision)

-- | Write multiple 'Line's in a batch.
--
-- This is more efficient than calling 'write' multiple times.
--
-- >>> let p = writeParams "test-db"
-- >>> :{
-- writeBatch p
--   [ Line @UTCTime "temp" (Map.singleton "city" "tokyo") (Map.fromList [("temp", FieldFloat 25.0)]) Nothing
--   , Line @UTCTime "temp" (Map.singleton "city" "osaka") (Map.fromList [("temp", FieldFloat 25.2)]) Nothing
--   ]
-- :}
writeBatch
  :: (Timestamp time, Foldable f)
  => WriteParams
  -> f (Line time)
  -> IO ()
writeBatch p@WriteParams {writePrecision} =
  writeByteString p . encodeLines (scaleTo writePrecision)

-- | Write a raw 'BL.ByteString'
writeByteString :: WriteParams -> BL.ByteString -> IO ()
writeByteString params payload = do
  manager' <- either HC.newManager return $ writeManager params
  response <- HC.httpLbs request manager' `catch` (throwIO . HTTPException)
  let body = HC.responseBody response
      status = HC.responseStatus response
  if BL.null body
    then do
      let message = B8.unpack $ HT.statusMessage status
      when (HT.statusIsServerError status) $
        throwIO $ ServerError message
      when (HT.statusIsClientError status) $
        throwIO $ ClientError message request
    else case A.eitherDecode' body of
      Left message ->
        throwIO $ UnexpectedResponse message request body
      Right val -> case A.parse parseErrorObject val of
        A.Success err ->
          fail $ "BUG: impossible code path in "
            ++ "Database.InfluxDB.Write.writeByteString: "
            ++ err
        A.Error message -> do
          when (HT.statusIsServerError status) $
            throwIO $ ServerError message
          when (HT.statusIsClientError status) $
            throwIO $ ClientError message request
          throwIO $ UnexpectedResponse
            ("BUG: " ++ message
              ++ " in Database.InfluxDB.Write.writeByteString")
            request
            (A.encode val)
  where
    request = (writeRequest params)
      { HC.requestBody = HC.RequestBodyLBS payload
      }

writeRequest :: WriteParams -> HC.Request
writeRequest WriteParams {..} =
  HC.setQueryString qs HC.defaultRequest
    { HC.host = TE.encodeUtf8 _host
    , HC.port = fromIntegral _port
    , HC.secure = _ssl
    , HC.method = "POST"
    , HC.path = "/write"
    }
  where
    Server {..} = writeServer
    qs = concat
      [ [ ("db", Just $ TE.encodeUtf8 $ databaseName writeDatabase)
        , ("precision", Just $ TE.encodeUtf8 $ precisionName writePrecision)
        ]
      , fromMaybe [] $ do
        Key name <- writeRetentionPolicy
        return [("rp", Just (TE.encodeUtf8 name))]
      , fromMaybe [] $ do
        Credentials { _user = u, _password = p } <- writeAuthentication
        return
          [ ("u", Just (TE.encodeUtf8 u))
          , ("p", Just (TE.encodeUtf8 p))
          ]
      ]

makeLensesWith
  ( lensRules
    & generateSignatures .~ False
    & lensField .~ lookingupNamer
      [ ("writeServer", "_server")
      , ("writeDatabase", "_database")
      , ("writeRetentionPolicy", "retentionPolicy")
      , ("writePrecision", "_precision")
      , ("writeManager", "_manager")
      , ("writeAuthentication", "_authentication")
      ]
    )
  ''WriteParams

-- |
-- >>> let p = writeParams "foo"
-- >>> p ^. server.host
-- "localhost"
instance HasServer WriteParams where
  server = _server

-- |
-- >>> let p = writeParams "foo"
-- >>> p ^. database
-- "foo"
instance HasDatabase WriteParams where
  database = _database

-- | Target retention policy for the write.
--
-- InfluxDB writes to the @default@ retention policy if this parameter is set
-- to 'Nothing'.
--
-- >>> let p = writeParams "foo" & retentionPolicy .~ Just "two_hours"
-- >>> p ^. retentionPolicy
-- Just "two_hours"
retentionPolicy :: Lens' WriteParams (Maybe Key)

-- |
-- >>> let p = writeParams "foo"
-- >>> p ^. precision
-- Nanosecond
instance HasPrecision 'WriteRequest WriteParams where
  precision = _precision

-- |
-- >>> let p = writeParams "foo" & manager .~ Left HC.defaultManagerSettings
instance HasManager WriteParams where
  manager = _manager

-- | Authentication info for the write
--
-- >>> let p = writeParams "foo"
-- >>> p ^. authentication
-- Nothing
-- >>> let p' = p & authentication ?~ credentials "john" "passw0rd"
-- >>> p' ^. authentication . traverse . user
-- "john"
instance HasCredentials WriteParams where
  authentication = _authentication
