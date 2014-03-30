{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.InfluxDB.Decode where
import Control.Applicative

import Database.InfluxDB.Types

class FromSeries a where
  parseSeries :: Series -> Parser a

instance FromSeries Series where
  parseSeries = return

instance FromSeries SeriesData where
  parseSeries = return . seriesData

fromSeries :: FromSeries a => Series -> Either String a
fromSeries = runParser . parseSeries

class FromSeriesData a where
  parseSeriesData :: SeriesData -> Parser a

instance FromSeriesData SeriesData where
  parseSeriesData = return

fromSeriesData :: FromSeriesData a => SeriesData -> Either String a
fromSeriesData = runParser . parseSeriesData

class FromValue a where
  parseValue :: Value -> Parser a

fromValue :: FromValue a => Value -> Either String a
fromValue = runParser . parseValue

instance FromValue Value where
  parseValue = return

instance FromValue () where
  parseValue Null = return ()
  parseValue v = typeMismatch "()" v

instance FromValue Bool where
  parseValue (Bool b) = return b
  parseValue v = typeMismatch "Bool" v

instance FromValue a => FromValue (Maybe a) where
  parseValue Null = return Nothing
  parseValue v = Just <$> parseValue v

instance FromValue Int where
  parseValue (Int n) = return $ fromIntegral n
  parseValue v = typeMismatch "Int" v

instance FromValue Double where
  parseValue (Float d) = return d
  parseValue v = typeMismatch "Float" v

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
