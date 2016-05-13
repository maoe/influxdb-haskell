module Database.InfluxDB.Ping (ping) where

import Database.InfluxDB.Types

ping :: Server -> IO Bool
ping Server {..} = do
