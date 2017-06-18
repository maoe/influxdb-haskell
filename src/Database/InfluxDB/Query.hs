{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
  ) where
import Control.Exception
import Control.Monad
import Data.Char
import Data.List

import Control.Lens
import Data.Aeson
import Data.Optional (Optional(..), optional)
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
--     String levelDesc <- getField "level_description" columns fields
--     String location <- getField "location" columns fields
--     FieldFloat waterLevel <-
--       getField "water_level" columns fields >>= parseQueryField
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

instance (a ~ Value, b ~ Value) => QueryResults (a, b) where
  parseResults _ = parseResultsWith $ \_ _ _ fields ->
    maybe (fail $ "invalid fields: " ++ show fields) return $ do
      a <- fields V.!? 0
      b <- fields V.!? 1
      return (a, b)

instance (a ~ Value, b ~ Value, c ~ Value)
  => QueryResults (a, b, c) where
    parseResults _ = parseResultsWith $ \_ _ _ fields ->
      maybe (fail $ "invalid fields: " ++ show fields) return $ do
        a <- fields V.!? 0
        b <- fields V.!? 1
        c <- fields V.!? 2
        return (a, b, c)

instance (a ~ Value, b ~ Value, c ~ Value, d ~ Value)
  => QueryResults (a, b, c, d) where
    parseResults _ = parseResultsWith $ \_ _ _ fields ->
      maybe (fail $ "invalid fields: " ++ show fields) return $ do
        a <- fields V.!? 0
        b <- fields V.!? 1
        c <- fields V.!? 2
        d <- fields V.!? 3
        return (a, b, c, d)

instance (a ~ Value, b ~ Value, c ~ Value, d ~ Value, e ~ Value)
  => QueryResults (a, b, c, d, e) where
    parseResults _ = parseResultsWith $ \_ _ _ fields ->
      maybe (fail $ "invalid fields: " ++ show fields) return $ do
        a <- fields V.!? 0
        b <- fields V.!? 1
        c <- fields V.!? 2
        d <- fields V.!? 3
        e <- fields V.!? 4
        return (a, b, c, d, e)

instance (a ~ Value, b ~ Value, c ~ Value, d ~ Value, e ~ Value, f ~ Value)
  => QueryResults (a, b, c, d, e, f) where
    parseResults _ = parseResultsWith $ \_ _ _ fields ->
      maybe (fail $ "invalid fields: " ++ show fields) return $ do
        a <- fields V.!? 0
        b <- fields V.!? 1
        c <- fields V.!? 2
        d <- fields V.!? 3
        e <- fields V.!? 4
        f <- fields V.!? 5
        return (a, b, c, d, e, f)

instance
  ( a ~ Value, b ~ Value, c ~ Value, d ~ Value, e ~ Value, f ~ Value
  , g ~ Value )
  => QueryResults (a, b, c, d, e, f, g) where
    parseResults _ = parseResultsWith $ \_ _ _ fields ->
      maybe (fail $ "invalid fields: " ++ show fields) return $ do
        a <- fields V.!? 0
        b <- fields V.!? 1
        c <- fields V.!? 2
        d <- fields V.!? 3
        e <- fields V.!? 4
        f <- fields V.!? 5
        g <- fields V.!? 6
        return (a, b, c, d, e, f, g)

instance
  ( a ~ Value, b ~ Value, c ~ Value, d ~ Value, e ~ Value, f ~ Value
  , g ~ Value, h ~ Value )
  => QueryResults (a, b, c, d, e, f, g, h) where
    parseResults _ = parseResultsWith $ \_ _ _ fields ->
      maybe (fail $ "invalid fields: " ++ show fields) return $ do
        a <- fields V.!? 0
        b <- fields V.!? 1
        c <- fields V.!? 2
        d <- fields V.!? 3
        e <- fields V.!? 4
        f <- fields V.!? 5
        g <- fields V.!? 6
        h <- fields V.!? 7
        return (a, b, c, d, e, f, g, h)

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
query :: QueryResults a => QueryParams -> Query -> IO (Vector a)
query params q = withQueryResponse params Nothing q go
  where
    go request response = do
      chunks <- HC.brConsume $ HC.responseBody response
      let body = BL.fromChunks chunks
      case eitherDecode' body of
        Left message -> throwIO $ UnexpectedResponse message body
        Right val -> case A.parse (parseResults (queryPrecision params)) val of
          A.Success vec -> return vec
          A.Error message ->
            errorQuery request response message

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
              throwIO $ UnexpectedResponse message $ BL.fromStrict unconsumed
            AB.Partial k' -> do
              chunk' <- HC.responseBody response
              loop x k' chunk'
            AB.Done leftover val ->
              case A.parse (parseResults (queryPrecision params)) val of
                A.Success vec -> do
                  x' <- step x vec
                  loop x' k0 leftover
                A.Error message ->
                  errorQuery request response message

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
queryRequest QueryParams {..} = HC.defaultRequest
  { HC.host = TE.encodeUtf8 _host
  , HC.port = fromIntegral _port
  , HC.secure = _ssl
  , HC.method = "GET"
  , HC.path = "/query"
  }
  where
    Server {..} = queryServer

errorQuery :: HC.Request -> HC.Response body -> String -> IO a
errorQuery request response message = do
  let status = HC.responseStatus response
  when (HT.statusIsServerError status) $
    throwIO $ ServerError message
  when (HT.statusIsClientError status) $
    throwIO $ ClientError message request
  fail $ "BUG: " ++ message ++ " in Database.InfluxDB.Query.query - "
    ++ show request

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
