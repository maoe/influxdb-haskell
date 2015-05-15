{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Database.InfluxDB.Decode
  ( FromSeries(..), fromSeries
  , FromSeriesData(..), fromSeriesData, fromSeriesData_
  , withValues, (.:), (.:?), (.!=)
  , FromValue(..), fromValue
  , Parser, ValueParser, typeMismatch
  ) where
import Control.Applicative
import Control.Monad.Reader
import Data.Either (rights)
import Data.Int
import Data.Word
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Vector (Vector)
import Data.Tuple (swap)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import Prelude

import Database.InfluxDB.Types

-- | A type that can be converted from a 'Series'.
class FromSeries a where
  parseSeries :: Series -> Parser a

instance FromSeries Series where
  parseSeries = return

instance FromSeries SeriesData where
  parseSeries = return . seriesData

-- | Converte a value from a 'Series', failing if the types do not match.
fromSeries :: FromSeries a => Series -> Either String a
fromSeries = runParser . parseSeries

-- | A type that can be converted from a 'SeriesData'. A typical implementation
-- is as follows.
--
-- > import Control.Applicative ((<$>), (<*>))
-- > import qualified Data.Vector as V
-- >
-- > data Event = Event Text EventType
-- > data EventType = Login | Logout
-- >
-- > instance FromSeriesData Event where
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

-- | Converte a value from a 'SeriesData', failing if the types do not match.
fromSeriesData :: FromSeriesData a => SeriesData -> Either String [a]
fromSeriesData SeriesData {..} = mapM
  (runParser . parseSeriesData seriesDataColumns)
  seriesDataPoints

-- | Same as @fromSeriesData@ but ignores parse errors and returns only
-- successful data.
fromSeriesData_ :: FromSeriesData a => SeriesData -> [a]
fromSeriesData_ SeriesData {..} = rights $ map
  (runParser . parseSeriesData seriesDataColumns)
  seriesDataPoints

-- | Helper function to define 'parseSeriesData' from 'ValueParser's.
withValues
  :: (Vector Value -> ValueParser a)
  -> Vector Column -> Vector Value -> Parser a
withValues f columns values =
  runReaderT m $ Map.fromList $ map swap $ V.toList $ V.indexed columns
  where
    ValueParser m = f values

-- | Retrieve the value associated with the given column. The result is 'empty'
-- if the column is not present or the value cannot be converted to the desired
-- type.
(.:) :: FromValue a => Vector Value -> Column -> ValueParser a
values .: column = do
  found <- asks $ Map.lookup column
  case found of
    Nothing -> liftParser $ parseError $ "No such column: " ++ T.unpack column
    Just idx -> do
      value <- V.indexM values idx
      liftParser $ parseValue value

-- | Retrieve the value associated with the given column. The result is
-- 'Nothing' if the column is not present or the value cannot be converted to
-- the desired type.
(.:?) :: FromValue a => Vector Value -> Column -> ValueParser (Maybe a)
values .:? column = do
  found <- asks $ Map.lookup column
  case found of
    Nothing -> return Nothing
    Just idx ->
      case values V.!? idx of
        Nothing -> return Nothing
        Just value -> liftParser $ parseValue value

-- | Helper for use in combination with '.:?' to provide default values for
-- optional columns.
(.!=) :: Parser (Maybe a) -> a -> Parser a
p .!= def = fromMaybe def <$> p

-- | A type that can be converted from a 'Value'.
class FromValue a where
  parseValue :: Value -> Parser a

-- | Converte a value from a 'Value', failing if the types do not match.
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
    | otherwise = parseError $ "Larger than the maximum Int8: " ++ show n
  parseValue v = typeMismatch "Int8" v

instance FromValue Int16 where
  parseValue (Int n)
    | n <= fromIntegral (maxBound :: Int16) = return $ fromIntegral n
    | otherwise = parseError $ "Larger than the maximum Int16: " ++ show n
  parseValue v = typeMismatch "Int16" v

instance FromValue Int32 where
  parseValue (Int n)
    | n <= fromIntegral (maxBound :: Int32) = return $ fromIntegral n
    | otherwise = parseError $ "Larger than the maximum Int32: " ++ show n
  parseValue v = typeMismatch "Int32" v

instance FromValue Int64 where
  parseValue (Int n)
    | n <= fromIntegral (maxBound :: Int64) = return $ fromIntegral n
    | otherwise = parseError $ "Larger than the maximum Int64: " ++ show n
  parseValue v = typeMismatch "Int64" v

instance FromValue Word8 where
  parseValue (Int n)
    | n <= fromIntegral (maxBound :: Word8) = return $ fromIntegral n
    | otherwise = parseError $ "Larger than the maximum Word8: " ++ show n
  parseValue v = typeMismatch "Word8" v

instance FromValue Word16 where
  parseValue (Int n)
    | n <= fromIntegral (maxBound :: Word16) = return $ fromIntegral n
    | otherwise = parseError $ "Larger than the maximum Word16: " ++ show n
  parseValue v = typeMismatch "Word16" v

instance FromValue Word32 where
  parseValue (Int n)
    | n <= fromIntegral (maxBound :: Word32) = return $ fromIntegral n
    | otherwise = parseError $ "Larger than the maximum Word32: " ++ show n
  parseValue v = typeMismatch "Word32" v

instance FromValue Double where
  parseValue (Float d) = return d
  -- If the floating number happens to be a whole number, it must
  -- have encoded as an integer. We should decode it back as a floating
  -- number here.
  parseValue (Int n) = return $ fromIntegral n
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
typeMismatch expected actual = parseError $
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

parseError :: String -> Parser a
parseError = Parser . Left
