module Database.InfluxDB.Encode where
import Data.Vector (Vector)
import qualified Data.DList as DL

import Database.InfluxDB.Types

class ToSeries a where
  toSeries :: a -> Series

class ToSeriesData a where
  toSeriesColumns :: a -> Vector Column
  toSeriesPoints :: a -> Vector Value

toSeriesData :: ToSeriesData a => a -> SeriesData
toSeriesData a = SeriesData
  { seriesDataColumns = toSeriesColumns a
  , seriesDataPoints = DL.singleton (toSeriesPoints a)
  }

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
