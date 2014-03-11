{-# LANGUAGE RankNTypes #-}
module Database.InfluxDB.Lens where
import Control.Applicative
import Data.ByteString (ByteString)

import Database.InfluxDB.Http

type Lens s t a b = Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

user :: Lens' (Settings a) ByteString
user f s = set <$> f (settingsUser s)
  where
    set u = s { settingsUser = u }

password :: Lens' (Settings a) ByteString
password f s = set <$> f (settingsPassword s)
  where
    set p = s { settingsPassword = p }

endpoint :: Lens' (Settings a) a
endpoint f s = set <$> f (settingsEndpoint s)
  where
    set a = s { settingsEndpoint = a }

host :: Lens' Server ByteString
host f s = set <$> f (serverHost s)
  where
    set h = s { serverHost = h }

port :: Lens' Server Int
port f s = set <$> f (serverPort s)
  where
    set p = s { serverPort = p }

server :: Lens' Database Server
server f s = set <$> f (databaseServer s)
  where
    set srv = s { databaseServer = srv }

database :: Lens' Database ByteString
database f s = set <$> f (databaseName s)
  where
    set name = s { databaseName = name }
