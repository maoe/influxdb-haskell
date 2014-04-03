module Database.InfluxDB.Stream where
import Prelude hiding (mapM)

-- | Effectful stream
data Stream m a
  = Yield a (m (Stream m a))
  -- ^ Yield a value. The stream will be continued.
  | Done
  -- ^ The end of the stream.

-- | Map each element of a stream to a monadic action, evaluate these actions
-- from left to right, and collect the results as a stream.
mapM :: Monad m => (a -> m b) -> Stream m a -> m (Stream m b)
mapM _ Done = return Done
mapM f (Yield a mb) = do
  a' <- f a
  b <- mb
  return $ Yield a' (mapM f b)
