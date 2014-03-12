{-# LANGUAGE RankNTypes #-}
module Database.InfluxDB.Lens where
import Control.Applicative
import Data.ByteString (ByteString)

import Database.InfluxDB.Http

type Lens s t a b = Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

credentials :: Lens' (Settings a) Credentials
credentials f s = set <$> f (settingsCreds s)
  where
    set c = s { settingsCreds = c }

user :: Lens' Credentials ByteString
user f s = set <$> f (credsUser s)
  where
    set u = s { credsUser = u }

password :: Lens' Credentials ByteString
password f s = set <$> f (credsPassword s)
  where
    set p = s { credsPassword = p }

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
