{-# LANGUAGE ScopedTypeVariables #-}
module Database.InfluxDB.Encode where
import Data.Proxy
import Data.Vector (Vector)
import qualified Data.DList as DL

import Database.InfluxDB.Types

-- | A type that can be converted to a @Series@.
class ToSeries a where
  toSeries :: a -> Series

-- | A type that can be converted to a @SeriesData@.
class ToSeriesData a where
  toSeriesColumns :: Proxy a -> Vector Column
  toSeriesPoints :: a -> Vector Value

toSeriesData :: forall a. ToSeriesData a => a -> SeriesData
toSeriesData a = SeriesData
  { seriesDataColumns = toSeriesColumns (Proxy :: Proxy a)
  , seriesDataPoints = DL.singleton (toSeriesPoints a)
  }

-- | A type that can be stored in InfluxDB.
class ToValue a where
  toValue :: a -> Value

instance ToValue () where
  toValue _ = Null

instance ToValue Bool where
  toValue = Bool

instance ToValue a => ToValue (Maybe a) where
  toValue Nothing = Null
  toValue (Just a) = toValue a

instance ToValue Int where
  toValue = Int . fromIntegral

instance ToValue Double where
  toValue = Float
