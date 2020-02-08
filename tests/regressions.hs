{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
import Data.Time.Clock (UTCTime)
import Test.Tasty
import Test.Tasty.HUnit
import Text.RawString.QQ (r)
import qualified Data.Map as M

import Database.InfluxDB
import Database.InfluxDB.Line
import qualified Database.InfluxDB.Format as F

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "regression tests"
  [ issue75
  ]

-- https://github.com/maoe/influxdb-haskell/issues/75

issue75 :: TestTree
issue75 = testCaseSteps "issue #75" $ \step -> do
  step "Checking encoded value"
  let string = [r|bl\"a|]
  let encoded = encodeLine (scaleTo Nanosecond)
        $ Line "testing" mempty
          (M.singleton "test" $ FieldString string)
          (Nothing :: Maybe UTCTime)
  encoded @?= [r|testing test="bl\\\"a"|]

  step "Preparing a test database"
  let db = "issue75"
  let p = queryParams db
  manage p $ formatQuery ("DROP DATABASE "%F.database) db
  manage p $ formatQuery ("CREATE DATABASE "%F.database) db

  step "Checking server response"
  let wp = writeParams db
  writeByteString wp encoded
