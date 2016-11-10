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
  ( Query
  , query
  , queryChunked

  -- * Query parameters
  , QueryParams
  , queryParams
  , Types.server
  , Types.database
  , Types.precision
  , authentication
  , Types.manager

  , QueryResults(..)
  , parseResultsWith
  , parseKey
  ) where
import Control.Applicative hiding (optional)
import Control.Exception
import Control.Monad
import Text.Printf

import Control.Lens
import Data.Aeson
import Data.Optional (Optional(..), optional)
import Data.Vector (Vector)
import qualified Control.Foldl as Fold
import qualified Data.Aeson.Parser as A
import qualified Data.Aeson.Types as A
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Network.HTTP.Types as HT

import Database.InfluxDB.JSON
import Database.InfluxDB.Types as Types
import qualified Database.InfluxDB.Format as F
import qualified Network.HTTP.Client.Compat as HC

class QueryResults a where
  parseResults
    :: Precision 'QueryRequest
    -> Value
    -> A.Parser (Vector a)

instance QueryResults () where
  parseResults _ val = success <|> parseErrorObject val
    where
      success = do
        results <- parseResultsObject val
        V.forM results $ A.withObject "{}" $ \obj ->
          guard $ obj == mempty

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
        h <- fields V.!? 6
        return (a, b, c, d, e, f, g, h)

parseKey :: Value -> Array -> Array -> A.Parser Key
parseKey name columns fields = do
  case V.elemIndex name columns >>= V.indexM fields of
    Just (String (F.formatKey F.text -> key)) -> return key
    _ -> fail $ printf "parseKey: %s not found in columns" $ show name

-- | The full set of parameters for the query API
data QueryParams = QueryParams
  { _server :: !Server
  , _database :: !Database
  , _precision :: !(Precision 'QueryRequest)
  -- ^ Timestamp precision
  --
  -- InfluxDB uses nanosecond precision if nothing is specified.
  , _authentication :: !(Maybe Credentials)
  -- ^ No authentication by default
  , _manager :: !(Either HC.ManagerSettings HC.Manager)
  -- ^ HTTP connection manager
  }

-- | Smart constructor for 'QueryParams'
--
-- Default parameters:
--
--   ['L.server'] 'localServer'
--   ['L.precision'] 'RFC3339'
--   ['authentication'] 'Nothing'
--   ['L.manager'] @'Left' 'HC.defaultManagerSettings'@
queryParams :: Database -> QueryParams
queryParams _database = QueryParams
  { _server = localServer
  , _precision = RFC3339
  , _authentication = Nothing
  , _manager = Left HC.defaultManagerSettings
  , ..
  }

-- | Query data from InfluxDB.
--
-- It may throw 'InfluxException'.
query :: QueryResults a => QueryParams -> Query -> IO (Vector a)
query params q = do
  manager' <- either HC.newManager return $ _manager params
  response <- HC.httpLbs request manager'
  let body = HC.responseBody response
  case eitherDecode' body of
    Left message ->
      throwIO $ IllformedJSON message body
    Right val -> case A.parse (parseResults (_precision params)) val of
      A.Success vec -> return vec
      A.Error message -> do
        let status = HC.responseStatus response
        when (HT.statusIsServerError status) $
          throwIO $ ServerError message
        when (HT.statusIsClientError status) $
          throwIO $ BadRequest message request
        fail $ "BUG: " ++ message ++ " in Database.InfluxDB.Query.query - "
          ++ show body
  where
    request =
      HC.setQueryString (setPrecision (_precision params) qs) $
        queryRequest params
    qs =
      [ ("q", Just $ F.fromQuery q)
      , ("db", Just $ TE.encodeUtf8 $ databaseName $ _database params)
      ]

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
  -> Fold.FoldM IO (Vector a) r
  -> IO r
queryChunked params chunkSize q
  (Fold.FoldM step initialize extract) = do
    manager' <- either HC.newManager return $ _manager params
    HC.withResponse request manager' $ \resp -> do
      x0 <- initialize
      chunk0 <- HC.responseBody resp
      x <- loop resp x0 k0 chunk0
      extract x
  where
    request =
      HC.setQueryString (setPrecision (_precision params) qs) $
        queryRequest params
    qs =
      [ ("q", Just $ F.fromQuery q)
      , ("db", Just $ TE.encodeUtf8 $ databaseName $ _database params)
      , ("chunked", Just $ optional "true" (decodeChunkSize . max 1) chunkSize)
      ]
      where
        decodeChunkSize = BL.toStrict . BB.toLazyByteString . BB.intDec
    k0 = AB.parse A.json
    loop resp x k chunk
      | B.null chunk = return x
      | otherwise = case k chunk of
        AB.Fail unconsumed _contexts message ->
          throwIO $ IllformedJSON message (BL.fromStrict unconsumed)
        AB.Partial k' -> do
          chunk' <- HC.responseBody resp
          loop resp x k' chunk'
        AB.Done leftover val ->
          case A.parse (parseResults (_precision params)) val of
            A.Success vec -> do
              x' <- step x vec
              loop resp x' k0 leftover
            A.Error message -> do
              let status = HC.responseStatus resp
              when (HT.statusIsServerError status) $
                throwIO $ ServerError message
              when (HT.statusIsClientError status) $
                throwIO $ BadRequest message request
              fail $ "BUG: " ++ message
                ++ " in Database.InfluxDB.Query.queryChunked"

queryRequest :: QueryParams -> HC.Request
queryRequest QueryParams {..} = HC.defaultRequest
  { HC.host = TE.encodeUtf8 _host
  , HC.port = fromIntegral _port
  , HC.secure = _ssl
  , HC.method = "GET"
  , HC.path = "/query"
  }
  where
    Server {..} = _server

makeLensesWith (lensRules & generateSignatures .~ False) ''QueryParams

server :: Lens' QueryParams Server

-- |
-- >>> let p = queryParams "foo"
-- >>> p ^. server.host
-- "localhost"
instance HasServer QueryParams where
  server = Database.InfluxDB.Query.server

database :: Lens' QueryParams Database

-- |
-- >>> let p = queryParams "foo"
-- >>> p ^. database
-- "foo"
instance HasDatabase QueryParams where
  database = Database.InfluxDB.Query.database

precision :: Lens' QueryParams (Precision 'QueryRequest)

-- | Returning JSON responses contain timestamps in the specified
-- precision/format.
--
-- >>> let p = queryParams "foo"
-- >>> p ^. precision
-- Nanosecond
instance HasPrecision 'QueryRequest QueryParams where
  precision = Database.InfluxDB.Query.precision

manager :: Lens' QueryParams (Either HC.ManagerSettings HC.Manager)

-- |
-- >>> let p = queryParams "foo"
-- >>> p & manager .~ Left HC.defaultManagerSettings
instance HasManager QueryParams where
  manager = Database.InfluxDB.Query.manager

-- | Authentication info for the query
--
-- >>> let p = queryParams "foo"
-- >>> p ^. authentication
-- Nothing
authentication :: Lens' QueryParams (Maybe Credentials)
