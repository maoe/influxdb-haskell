{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Database.InfluxDB.Decode
  ( FromSeries(..), fromSeries
  , FromSeriesData(..), fromSeriesData
  , withValues, (.:)
  , FromValue(..), fromValue
  , Parser, ValueParser, typeMismatch
  ) where
import Control.Applicative
import Control.Monad.Reader
import Data.Int
import Data.Word
import Data.Map (Map)
import Data.Vector (Vector)
import Data.Tuple (swap)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V

import Database.InfluxDB.Types

-- | A type that can be converted from a @Series@.
class FromSeries a where
  parseSeries :: Series -> Parser a

instance FromSeries Series where
  parseSeries = return

instance FromSeries SeriesData where
  parseSeries = return . seriesData

-- | Converte a value from a @Series@, failing if the types do not match.
fromSeries :: FromSeries a => Series -> Either String a
fromSeries = runParser . parseSeries

-- | A type that can be converted from a @SeriesData@. A typical implementation
-- is as follows.
--
-- > import Control.Applicative ((<$>), (<*>))
-- > import qualified Data.Vector as V
-- >
-- > data Event = Event Text EventType
-- > data EventType = Login | Logout
-- >
-- > instance FromSeriesData where
-- >   parseSeriesData = withValues $ \values -> Event
-- >     <$> values .: "user"
-- >     <*> values .: "type"
-- >
-- > instance FromValue EventType
class FromSeriesData a where
  parseSeriesData :: Vector Column -> Vector Value -> Parser a

instance FromSeriesData SeriesData where
  parseSeriesData columns values = return SeriesData
    { seriesDataColumns = columns
    , seriesDataPoints = [values]
    }

-- | Converte a value from a @SeriesData@, failing if the types do not match.
fromSeriesData :: FromSeriesData a => SeriesData -> Either String [a]
fromSeriesData SeriesData {..} = mapM
  (runParser . parseSeriesData seriesDataColumns)
  seriesDataPoints

withValues
  :: (Vector Value -> ValueParser a)
  -> Vector Column -> Vector Value -> Parser a
withValues f columns values =
  runReaderT m $ Map.fromList $ map swap $ V.toList $ V.indexed columns
  where
    ValueParser m = f values

(.:) :: FromValue a => Vector Value -> Column -> ValueParser a
values .: column = do
  found <- asks $ Map.lookup column
  case found of
    Nothing -> fail $ "No such column: " ++ T.unpack column
    Just idx -> do
      value <- V.indexM values idx
      liftParser $ parseValue value

-- | A type that can be converted from a @Value@.
class FromValue a where
  parseValue :: Value -> Parser a

-- | Converte a value from a @Value@, failing if the types do not match.
fromValue :: FromValue a => Value -> Either String a
fromValue = runParser . parseValue

instance FromValue Value where
  parseValue = return

instance FromValue Bool where
  parseValue (Bool b) = return b
  parseValue v = typeMismatch "Bool" v

instance FromValue a => FromValue (Maybe a) where
  parseValue Null = return Nothing
  parseValue v = Just <$> parseValue v

instance FromValue Int where
  parseValue (Int n) = return $ fromIntegral n
  parseValue v = typeMismatch "Int" v

instance FromValue Int8 where
  parseValue (Int n)
    | n <= fromIntegral (maxBound :: Int8) = return $ fromIntegral n
    | otherwise = fail $ "Larger than the maximum Int8: " ++ show n
  parseValue v = typeMismatch "Int8" v

instance FromValue Int16 where
  parseValue (Int n)
    | n <= fromIntegral (maxBound :: Int16) = return $ fromIntegral n
    | otherwise = fail $ "Larger than the maximum Int16: " ++ show n
  parseValue v = typeMismatch "Int16" v

instance FromValue Int32 where
  parseValue (Int n)
    | n <= fromIntegral (maxBound :: Int32) = return $ fromIntegral n
    | otherwise = fail $ "Larger than the maximum Int32: " ++ show n
  parseValue v = typeMismatch "Int32" v

instance FromValue Int64 where
  parseValue (Int n)
    | n <= fromIntegral (maxBound :: Int64) = return $ fromIntegral n
    | otherwise = fail $ "Larger than the maximum Int64: " ++ show n
  parseValue v = typeMismatch "Int64" v

instance FromValue Word8 where
  parseValue (Int n)
    | n <= fromIntegral (maxBound :: Word8) = return $ fromIntegral n
    | otherwise = fail $ "Larger than the maximum Word8: " ++ show n
  parseValue v = typeMismatch "Word8" v

instance FromValue Word16 where
  parseValue (Int n)
    | n <= fromIntegral (maxBound :: Word16) = return $ fromIntegral n
    | otherwise = fail $ "Larger than the maximum Word16: " ++ show n
  parseValue v = typeMismatch "Word16" v

instance FromValue Word32 where
  parseValue (Int n)
    | n <= fromIntegral (maxBound :: Word32) = return $ fromIntegral n
    | otherwise = fail $ "Larger than the maximum Word32: " ++ show n
  parseValue v = typeMismatch "Word32" v

instance FromValue Double where
  parseValue (Float d) = return d
  parseValue v = typeMismatch "Float" v

instance FromValue T.Text where
  parseValue (String xs) = return xs
  parseValue v = typeMismatch "Text" v

instance FromValue TL.Text where
  parseValue (String xs) = return $ TL.fromStrict xs
  parseValue v = typeMismatch "lazy Text" v

instance FromValue String where
  parseValue (String xs) = return $ T.unpack xs
  parseValue v = typeMismatch "String" v

typeMismatch
  :: String
  -> Value
  -> Parser a
typeMismatch expected actual = fail $
  "when expecting a " ++ expected ++
  ", encountered " ++ name ++ " instead"
  where
    name = case actual of
      Int _ -> "Int"
      Float _ -> "Float"
      String _ -> "String"
      Bool _ -> "Bool"
      Null -> "Null"

newtype Parser a = Parser
  { runParser :: Either String a
  } deriving (Functor, Applicative, Monad)

type ColumnIndex = Map Column Int

newtype ValueParser a = ValueParser (ReaderT ColumnIndex Parser a)
  deriving (Functor, Applicative, Monad, MonadReader ColumnIndex)

liftParser :: Parser a -> ValueParser a
liftParser = ValueParser . ReaderT . const
