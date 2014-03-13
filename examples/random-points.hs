{-# LANGUAGE OverloadedStrings #-}
import qualified Network.HTTP.Client as HC
import Data.Text (Text)

import Database.InfluxDB.Http
import Database.InfluxDB.Lens

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  HC.withManager HC.defaultManagerSettings $ \manager -> do
    dropDatabase config manager $ Database "random-points" Nothing
    db <- createDatabase config manager "random-points"
    return ()

data D = D
  { foo, bar, baz, quu, qux :: Int
  }

config :: Config
config = Config
  { configCreds = rootCreds
  , configServer = localServer
  }