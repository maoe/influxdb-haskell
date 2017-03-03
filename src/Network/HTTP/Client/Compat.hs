{-# LANGUAGE CPP #-}
module Network.HTTP.Client.Compat
  ( defaultRequest
  , module X
  ) where

#if MIN_VERSION_http_client(0, 5, 0)
import Network.HTTP.Client (defaultRequest)
import Network.HTTP.Client as X hiding (defaultRequest)
#else
import Data.Default.Class (def)
import Network.HTTP.Client as X

defaultRequest :: Request
defaultRequest = def
#endif
