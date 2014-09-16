{-# LANGUAGE RankNTypes #-}
module Database.InfluxDB.Lens
  ( Lens, Lens'

  -- * Lenses for 'Config'
  , credentials
  , httpManager

  -- * Lenses for 'Credentials'
  , user, password

  -- * Lenses for 'Server'
  , host, port, ssl
  ) where
import Control.Applicative
import Data.Text (Text)

import Network.HTTP.Client (Manager)

import Database.InfluxDB.Http

type Lens s t a b = Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

-- | User credentials for authentication
credentials :: Lens' Config Credentials
credentials f r = set <$> f (configCreds r)
  where
    set c = r { configCreds = c }

-- | An instance of 'Manager' from @http-client@ package
httpManager :: Lens' Config Manager
httpManager f c = set <$> f (configHttpManager c)
  where
    set m = c { configHttpManager = m }

-- | User name to be used for authentication
user :: Lens' Credentials Text
user f c = set <$> f (credsUser c)
  where
    set u = c { credsUser = u }

-- | Password to be used for authentication
password :: Lens' Credentials Text
password f s = set <$> f (credsPassword s)
  where
    set p = s { credsPassword = p }

-- | Host name or IP address of an InfluxDB
host :: Lens' Server Text
host f s = set <$> f (serverHost s)
  where
    set h = s { serverHost = h }

-- | Port number to be used to connect to an InfluxDB
port :: Lens' Server Int
port f s = set <$> f (serverPort s)
  where
    set p = s { serverPort = p }

-- | Whether or not to enable SSL connection
ssl :: Lens' Server Bool
ssl f s = set <$> f (serverSsl s)
  where
    set s' = s { serverSsl = s' }
