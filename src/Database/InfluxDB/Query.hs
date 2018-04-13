{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module Database.InfluxDB.Query
  (
  -- * Query interface
    Query
  , query
  , queryChunked

  -- * Query parameters
  , QueryParams
  , queryParams
  , server
  , database
  , precision
  , manager

  -- * Parsing results
  , QueryResults(..)
  , parseResultsWith

  -- * Low-level functions
  , withQueryResponse

  -- * Re-exports from tagged
  , Tagged(..)
  , untag
  ) where
import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import Data.Proxy
import GHC.TypeLits

import Control.Lens
import Data.Aeson
import Data.Optional (Optional(..), optional)
import Data.Tagged
import Data.Vector (Vector)
import Data.Void
import qualified Control.Foldl as L
import qualified Data.Aeson.Parser as A
import qualified Data.Aeson.Types as A
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Types as HT

import Database.InfluxDB.JSON
import Database.InfluxDB.Types as Types
import qualified Database.InfluxDB.Format as F

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XRecordWildCards
-- >>> import Data.Time (UTCTime)

-- | Types that can be converted from an JSON object returned by InfluxDB.
--
-- For example the @h2o_feet@ series in
-- [the official document](https://docs.influxdata.com/influxdb/v1.2/query_language/data_exploration/)
-- can be encoded as follows:
--
-- >>> :{
-- data H2OFeet = H2OFeet
--   { time :: UTCTime
--   , levelDesc :: T.Text
--   , location :: T.Text
--   , waterLevel :: Double
--   }
-- instance QueryResults H2OFeet where
--   parseResults prec = parseResultsWith $ \_ _ columns fields -> do
--     time <- getField "time" columns fields >>= parseUTCTime prec
--     levelDesc <- getField "level_description" columns fields >>= parseJSON
--     location <- getField "location" columns fields >>= parseJSON
--     waterLevel <- getField "water_level" columns fields >>= parseJSON
--     return H2OFeet {..}
-- :}
class QueryResults a where
  -- | Parse a JSON object as an array of values of expected type.
  parseResults
    :: Precision 'QueryRequest
    -> Value
    -> A.Parser (Vector a)

instance QueryResults Void where
  parseResults _ = A.withObject "error" $ \obj -> obj .:? "error"
    >>= maybe (pure V.empty) (withText "error" $ fail . T.unpack)

fieldName :: KnownSymbol k => proxy k -> T.Text
fieldName = T.pack . symbolVal

-- | One-off type for non-timestamped measurements
--
-- >>> let p = queryParams "_internal"
-- >>> dbs <- query p "SHOW DATABASES" :: IO (V.Vector (Tagged "name" T.Text))
-- >>> find ((== "_internal") . untag) dbs
-- Just (Tagged "_internal")
instance (KnownSymbol k, FromJSON v) => QueryResults (Tagged k v) where
  parseResults _ = parseResultsWith $ \_ _ columns fields ->
    getField (fieldName (Proxy :: Proxy k)) columns fields >>= parseJSON

-- | One-off tuple for sigle-field measurements
instance
  ( KnownSymbol k1, FromJSON v1
  , KnownSymbol k2, FromJSON v2 )
  => QueryResults (Tagged k1 v1, Tagged k2 v2) where
    parseResults _ = parseResultsWith $ \_ _ columns fields -> do
      v1 <- parseJSON
        =<< getField (fieldName (Proxy :: Proxy k1)) columns fields
      v2 <- parseJSON
        =<< getField (fieldName (Proxy :: Proxy k2)) columns fields
      return (v1, v2)

-- | One-off tuple for two-field measurements
instance
  ( KnownSymbol k1, FromJSON v1
  , KnownSymbol k2, FromJSON v2
  , KnownSymbol k3, FromJSON v3 )
  => QueryResults (Tagged k1 v1, Tagged k2 v2, Tagged k3 v3) where
    parseResults _ = parseResultsWith $ \_ _ columns fields -> do
      v1 <- parseJSON
        =<< getField (fieldName (Proxy :: Proxy k1)) columns fields
      v2 <- parseJSON
        =<< getField (fieldName (Proxy :: Proxy k2)) columns fields
      v3 <- parseJSON
        =<< getField (fieldName (Proxy :: Proxy k3)) columns fields
      return (v1, v2, v3)

-- | One-off tuple for three-field measurements
instance
  ( KnownSymbol k1, FromJSON v1
  , KnownSymbol k2, FromJSON v2
  , KnownSymbol k3, FromJSON v3
  , KnownSymbol k4, FromJSON v4 )
  => QueryResults (Tagged k1 v1, Tagged k2 v2, Tagged k3 v3, Tagged k4 v4) where
    parseResults _ = parseResultsWith $ \_ _ columns fields -> do
      v1 <- parseJSON
        =<< getField (fieldName (Proxy :: Proxy k1)) columns fields
      v2 <- parseJSON
        =<< getField (fieldName (Proxy :: Proxy k2)) columns fields
      v3 <- parseJSON
        =<< getField (fieldName (Proxy :: Proxy k3)) columns fields
      v4 <- parseJSON
        =<< getField (fieldName (Proxy :: Proxy k4)) columns fields
      return (v1, v2, v3, v4)

-- | One-off tuple for four-field measurements
instance
  ( KnownSymbol k1, FromJSON v1
  , KnownSymbol k2, FromJSON v2
  , KnownSymbol k3, FromJSON v3
  , KnownSymbol k4, FromJSON v4
  , KnownSymbol k5, FromJSON v5 )
  => QueryResults
    ( Tagged k1 v1, Tagged k2 v2, Tagged k3 v3, Tagged k4 v4
    , Tagged k5 v5
    ) where
    parseResults _ = parseResultsWith $ \_ _ columns fields -> do
      v1 <- parseJSON
        =<< getField (fieldName (Proxy :: Proxy k1)) columns fields
      v2 <- parseJSON
        =<< getField (fieldName (Proxy :: Proxy k2)) columns fields
      v3 <- parseJSON
        =<< getField (fieldName (Proxy :: Proxy k3)) columns fields
      v4 <- parseJSON
        =<< getField (fieldName (Proxy :: Proxy k4)) columns fields
      v5 <- parseJSON
        =<< getField (fieldName (Proxy :: Proxy k5)) columns fields
      return (v1, v2, v3, v4, v5)

-- | One-off tuple for five-field measurements
instance
  ( KnownSymbol k1, FromJSON v1
  , KnownSymbol k2, FromJSON v2
  , KnownSymbol k3, FromJSON v3
  , KnownSymbol k4, FromJSON v4
  , KnownSymbol k5, FromJSON v5
  , KnownSymbol k6, FromJSON v6 )
  => QueryResults
    ( Tagged k1 v1, Tagged k2 v2, Tagged k3 v3, Tagged k4 v4
    , Tagged k5 v5, Tagged k6 v6
    ) where
    parseResults _ = parseResultsWith $ \_ _ columns fields -> do
      v1 <- parseJSON
        =<< getField (fieldName (Proxy :: Proxy k1)) columns fields
      v2 <- parseJSON
        =<< getField (fieldName (Proxy :: Proxy k2)) columns fields
      v3 <- parseJSON
        =<< getField (fieldName (Proxy :: Proxy k3)) columns fields
      v4 <- parseJSON
        =<< getField (fieldName (Proxy :: Proxy k4)) columns fields
      v5 <- parseJSON
        =<< getField (fieldName (Proxy :: Proxy k5)) columns fields
      v6 <- parseJSON
        =<< getField (fieldName (Proxy :: Proxy k6)) columns fields
      return (v1, v2, v3, v4, v5, v6)

-- | One-off tuple for six-field measurement
instance
  ( KnownSymbol k1, FromJSON v1
  , KnownSymbol k2, FromJSON v2
  , KnownSymbol k3, FromJSON v3
  , KnownSymbol k4, FromJSON v4
  , KnownSymbol k5, FromJSON v5
  , KnownSymbol k6, FromJSON v6
  , KnownSymbol k7, FromJSON v7 )
  => QueryResults
    ( Tagged k1 v1, Tagged k2 v2, Tagged k3 v3, Tagged k4 v4
    , Tagged k5 v5, Tagged k6 v6, Tagged k7 v7
    ) where
    parseResults _ = parseResultsWith $ \_ _ columns fields -> do
      v1 <- parseJSON
        =<< getField (fieldName (Proxy :: Proxy k1)) columns fields
      v2 <- parseJSON
        =<< getField (fieldName (Proxy :: Proxy k2)) columns fields
      v3 <- parseJSON
        =<< getField (fieldName (Proxy :: Proxy k3)) columns fields
      v4 <- parseJSON
        =<< getField (fieldName (Proxy :: Proxy k4)) columns fields
      v5 <- parseJSON
        =<< getField (fieldName (Proxy :: Proxy k5)) columns fields
      v6 <- parseJSON
        =<< getField (fieldName (Proxy :: Proxy k6)) columns fields
      v7 <- parseJSON
        =<< getField (fieldName (Proxy :: Proxy k7)) columns fields
      return (v1, v2, v3, v4, v5, v6, v7)

-- | One-off tuple for seven-field measurements
instance
  ( KnownSymbol k1, FromJSON v1
  , KnownSymbol k2, FromJSON v2
  , KnownSymbol k3, FromJSON v3
  , KnownSymbol k4, FromJSON v4
  , KnownSymbol k5, FromJSON v5
  , KnownSymbol k6, FromJSON v6
  , KnownSymbol k7, FromJSON v7
  , KnownSymbol k8, FromJSON v8 )
  => QueryResults
    ( Tagged k1 v1, Tagged k2 v2, Tagged k3 v3, Tagged k4 v4
    , Tagged k5 v5, Tagged k6 v6, Tagged k7 v7, Tagged k8 v8
    ) where
    parseResults _ = parseResultsWith $ \_ _ columns fields -> do
      v1 <- parseJSON
        =<< getField (fieldName (Proxy :: Proxy k1)) columns fields
      v2 <- parseJSON
        =<< getField (fieldName (Proxy :: Proxy k2)) columns fields
      v3 <- parseJSON
        =<< getField (fieldName (Proxy :: Proxy k3)) columns fields
      v4 <- parseJSON
        =<< getField (fieldName (Proxy :: Proxy k4)) columns fields
      v5 <- parseJSON
        =<< getField (fieldName (Proxy :: Proxy k5)) columns fields
      v6 <- parseJSON
        =<< getField (fieldName (Proxy :: Proxy k6)) columns fields
      v7 <- parseJSON
        =<< getField (fieldName (Proxy :: Proxy k7)) columns fields
      v8 <- parseJSON
        =<< getField (fieldName (Proxy :: Proxy k8)) columns fields
      return (v1, v2, v3, v4, v5, v6, v7, v8)

-- | The full set of parameters for the query API
data QueryParams = QueryParams
  { queryServer :: !Server
  , queryDatabase :: !Database
  , queryPrecision :: !(Precision 'QueryRequest)
  -- ^ Timestamp precision
  --
  -- InfluxDB uses nanosecond precision if nothing is specified.
  , queryAuthentication :: !(Maybe Credentials)
  -- ^ No authentication by default
  , queryManager :: !(Either HC.ManagerSettings HC.Manager)
  -- ^ HTTP connection manager
  }

-- | Smart constructor for 'QueryParams'
--
-- Default parameters:
--
--   ['L.server'] 'defaultServer'
--   ['L.precision'] 'RFC3339'
--   ['L.authentication'] 'Nothing'
--   ['L.manager'] @'Left' 'HC.defaultManagerSettings'@
queryParams :: Database -> QueryParams
queryParams queryDatabase = QueryParams
  { queryServer = defaultServer
  , queryPrecision = RFC3339
  , queryAuthentication = Nothing
  , queryManager = Left HC.defaultManagerSettings
  , ..
  }

-- | Query data from InfluxDB.
--
-- It may throw 'InfluxException'.
--
-- If you need a lower-level interface (e.g. to bypass the 'QueryResults'
-- constraint etc), see 'withQueryResponse'.
query :: QueryResults a => QueryParams -> Query -> IO (Vector a)
query params q = withQueryResponse params Nothing q go
  where
    go request response = do
      chunks <- HC.brConsume $ HC.responseBody response
      let body = BL.fromChunks chunks
      case eitherDecode' body of
        Left message -> throwIO $ UnexpectedResponse message request body
        Right val -> case A.parse (parseResults (queryPrecision params)) val of
          A.Success vec -> return vec
          A.Error message -> errorQuery message request response val

setPrecision
  :: Precision 'QueryRequest
  -> [(B.ByteString, Maybe B.ByteString)]
  -> [(B.ByteString, Maybe B.ByteString)]
setPrecision prec qs = maybe qs (\p -> ("epoch", Just p):qs) $
  precisionParam prec

precisionParam :: Precision 'QueryRequest -> Maybe B.ByteString
precisionParam = \case
  Nanosecond -> return "ns"
  Microsecond -> return "u"
  Millisecond -> return "ms"
  Second -> return "s"
  Minute -> return "m"
  Hour -> return "h"
  RFC3339 -> Nothing

-- | Same as 'query' but it instructs InfluxDB to stream chunked responses
-- rather than returning a huge JSON object. This can be lot more efficient than
-- 'query' if the result is huge.
--
-- It may throw 'InfluxException'.
--
-- If you need a lower-level interface (e.g. to bypass the 'QueryResults'
-- constraint etc), see 'withQueryResponse'.
queryChunked
  :: QueryResults a
  => QueryParams
  -> Optional Int
  -- ^ Chunk size
  --
  -- By 'Default', InfluxDB chunks responses by series or by every 10,000
  -- points, whichever occurs first. If it set to a 'Specific' value, InfluxDB
  -- chunks responses by series or by that number of points.
  -> Query
  -> L.FoldM IO (Vector a) r
  -> IO r
queryChunked params chunkSize q (L.FoldM step initialize extract) =
  withQueryResponse params (Just chunkSize) q go
  where
    go request response = do
      x0 <- initialize
      chunk0 <- HC.responseBody response
      x <- loop x0 k0 chunk0
      extract x
      where
        k0 = AB.parse A.json
        loop x k chunk
          | B.null chunk = return x
          | otherwise = case k chunk of
            AB.Fail unconsumed _contexts message ->
              throwIO $ UnexpectedResponse message request $
                BL.fromStrict unconsumed
            AB.Partial k' -> do
              chunk' <- HC.responseBody response
              loop x k' chunk'
            AB.Done leftover val ->
              case A.parse (parseResults (queryPrecision params)) val of
                A.Success vec -> do
                  x' <- step x vec
                  loop x' k0 leftover
                A.Error message -> errorQuery message request response val

-- | Lower-level interface to query data.
withQueryResponse
  :: QueryParams
  -> Maybe (Optional Int)
  -- ^ Chunk size
  --
  -- By 'Nothing', InfluxDB returns all matching data points at once.
  -- By @'Just' 'Default'@, InfluxDB chunks responses by series or by every
  -- 10,000 points, whichever occurs first. If it set to a 'Specific' value,
  -- InfluxDB chunks responses by series or by that number of points.
  -> Query
  -> (HC.Request -> HC.Response HC.BodyReader -> IO r)
  -> IO r
withQueryResponse params chunkSize q f = do
    manager' <- either HC.newManager return $ queryManager params
    HC.withResponse request manager' (f request)
      `catch` (throwIO . HTTPException)
  where
    request =
      HC.setQueryString (setPrecision (queryPrecision params) queryString) $
        queryRequest params
    queryString = addChunkedParam
      [ ("q", Just $ F.fromQuery q)
      , ("db", Just db)
      ]
      where
        !db = TE.encodeUtf8 $ databaseName $ queryDatabase params
    addChunkedParam ps = case chunkSize of
      Nothing -> ps
      Just size ->
        let !chunked = optional "true" (decodeChunkSize . max 1) size
        in ("chunked", Just chunked) : ps
      where
        decodeChunkSize = BL.toStrict . BB.toLazyByteString . BB.intDec


queryRequest :: QueryParams -> HC.Request
queryRequest QueryParams {..} = applyBasicAuth $ HC.defaultRequest
  { HC.host = TE.encodeUtf8 _host
  , HC.port = fromIntegral _port
  , HC.secure = _ssl
  , HC.method = "GET"
  , HC.path = "/query"
  }
  where
    Server {..} = queryServer
    applyBasicAuth =
      case queryAuthentication of
        Nothing -> id
        Just (Credentials {..}) ->
          HC.applyBasicAuth (TE.encodeUtf8 _user) (TE.encodeUtf8 _password)

errorQuery :: String -> HC.Request -> HC.Response body -> A.Value -> IO a
errorQuery message request response val = do
  let status = HC.responseStatus response
  when (HT.statusIsServerError status) $
    throwIO $ ServerError message
  when (HT.statusIsClientError status) $
    throwIO $ ClientError message request
  throwIO $ UnexpectedResponse
    ("BUG: " ++ message ++ " in Database.InfluxDB.Query.query")
    request
    (encode val)

makeLensesWith
  ( lensRules
    & lensField .~ mappingNamer
      (\name -> case stripPrefix "query" name of
        Just (c:cs) -> ['_':toLower c:cs]
        _ -> [])
    )
  ''QueryParams

-- |
-- >>> let p = queryParams "foo"
-- >>> p ^. server.host
-- "localhost"
instance HasServer QueryParams where
  server = _server

-- |
-- >>> let p = queryParams "foo"
-- >>> p ^. database
-- "foo"
instance HasDatabase QueryParams where
  database = _database

-- | Returning JSON responses contain timestamps in the specified
-- precision/format.
--
-- >>> let p = queryParams "foo"
-- >>> p ^. precision
-- RFC3339
instance HasPrecision 'QueryRequest QueryParams where
  precision = _precision

-- |
-- >>> let p = queryParams "foo" & manager .~ Left HC.defaultManagerSettings
instance HasManager QueryParams where
  manager = _manager

-- | Authentication info for the query
--
-- >>> let p = queryParams "foo"
-- >>> p ^. authentication
-- Nothing
-- >>> let p' = p & authentication ?~ credentials "john" "passw0rd"
-- >>> p' ^. authentication.traverse.user
-- "john"
instance HasCredentials QueryParams where
  authentication = _authentication
