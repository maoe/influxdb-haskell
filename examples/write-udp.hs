{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Exception

import Control.Lens
import Data.Time.Clock
import Network.Socket

import Database.InfluxDB
import qualified Database.InfluxDB.Write.UDP as UDP

main :: IO ()
main = bracket (socket AF_INET Datagram defaultProtocol) close $ \sock -> do
  let localhost = tupleToHostAddress (127, 0, 0, 1)
  let params = UDP.writeParams sock $ SockAddrInet 8089 localhost
      tags1 =
          [ ("tag1", "A")
          , ("tag2", "B")
          ]
      fields1 =
        [ ("val1", FieldInt 10)
        , ("val2", FieldBool True)
        ]
      fields2 =
        [ ("val1", FieldInt 1)
        , ("val2", FieldBool False)
        ]
  UDP.write params $
    Line "measurement1" tags1 fields1 (Nothing :: Maybe UTCTime)
  now <- getCurrentTime
  UDP.write
    (params & UDP.precision .~ Millisecond)
    (Line "measurement1" tags1 fields2 (Just now))
