module Database.InfluxDB.Stream where
import Prelude hiding (mapM)

-- | Effectful stream type
data Stream m a
  = Yield a (m (Stream m a))
  | Done

mapM :: Monad m => (a -> m b) -> Stream m a -> m (Stream m b)
mapM _ Done = return Done
mapM f (Yield a mb) = do
  a' <- f a
  b <- mb
  return $ Yield a' (mapM f b)
