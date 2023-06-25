{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
import Control.Exception (bracket_, try)

import Control.Lens
import Data.Time
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as M
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Text.RawString.QQ as Raw (r)

import Database.InfluxDB
import Database.InfluxDB.Line
import qualified Database.InfluxDB.Format as F

main :: IO ()
main = defaultMain $ testGroup "regression tests"
  [ testCase "issue #64" case_issue64
  , testCase "issue #66" case_issue66
  , testCaseSteps "issue #75" case_issue75
  , testCaseSteps "issue #79" case_issue79
  ]

-- https://github.com/maoe/influxdb-haskell/issues/64
case_issue64 :: Assertion
case_issue64 = withDatabase dbName $ do
  write wp $ Line @UTCTime "count" Map.empty
    (Map.fromList [("value", FieldInt 1)])
    Nothing
  r <- try $ query qp "SELECT value FROM count"
  case r of
    Left err -> case err of
      UnexpectedResponse message _ _ ->
        message `elem` [
          "BUG: parsing Int failed, expected Number, but encountered String in Database.InfluxDB.Query.query",
          "BUG: expected Int, encountered String in Database.InfluxDB.Query.query"
        ] @? "Correct error message."
      _ ->
        assertFailure $ got ++ show err
    Right (v :: (V.Vector (Tagged "time" Int, Tagged "value" Int))) ->
      -- NOTE: The time columns should be UTCTime, Text, or String
      assertFailure $ got ++ "no errors: " ++ show v
  where
    dbName = "case_issue64"
    qp = queryParams dbName & precision .~ RFC3339
    wp = writeParams dbName
    got = "expeted an UnexpectedResponse but got "

-- https://github.com/maoe/influxdb-haskell/issues/66
case_issue66 :: Assertion
case_issue66 = do
  r <- try $ query (queryParams "_internal") "SELECT time FROM dummy"
  case r of
    Left err -> case err of
      UnexpectedResponse message _ _ ->
        message @?=
          "BUG: at least 1 non-time field must be queried in Database.InfluxDB.Query.query"
      _ ->
        assertFailure $ got ++ show err
    Right (v :: V.Vector (Tagged "time" Int)) ->
      assertFailure $ got ++ "no errors: " ++ show v
  where
    got = "expected an UnexpectedResponse but got "

-- https://github.com/maoe/influxdb-haskell/issues/75
case_issue75 :: (String -> IO ()) -> Assertion
case_issue75 step = do
  step "Checking encoded value"
  let string = [Raw.r|bl\"a|]
  let encoded = encodeLine (scaleTo Nanosecond)
        $ Line @UTCTime "testing" mempty
          (M.singleton "test" $ FieldString string)
          Nothing
  encoded @?= [Raw.r|testing test="bl\\\"a"|]

  step "Preparing a test database"
  let db = "issue75"
  let p = queryParams db
  manage p $ formatQuery ("DROP DATABASE "%F.database) db
  manage p $ formatQuery ("CREATE DATABASE "%F.database) db

  step "Checking server response"
  let wp = writeParams db
  writeByteString wp encoded

case_issue79 :: (String -> IO ()) -> Assertion
case_issue79 step = withDatabase db $ do
  let w = writeParams db
  let q = queryParams db
  step "Querying an empty series with two fields expected"
  _ <- query @(Tagged "time" UTCTime, Tagged "value" Int) q "SELECT * FROM foo"
  step "Querying an empty series with the results ignored"
  _ <- query @Ignored q "SELECT * FROM foo"
  step "Querying an empty series expecting an empty result"
  _ <- query @Empty q "SELECT * FROM foo"
  step "Writing a data point"
  write w $ Line @UTCTime "foo" mempty (Map.fromList [("value", FieldInt 42)]) Nothing
  step "Querying a non-empty series with two fields expected"
  _ <- query @(Tagged "time" UTCTime, Tagged "value" Int) q "SELECT * FROM foo"
  step "Querying a non-empty series with the results ignored"
  _ <- query @Ignored q "SELECT * FROM foo"
  return ()
  where
    db = "case_issue79"

withDatabase :: Database -> IO a -> IO a
withDatabase dbName f = bracket_
  (manage q (formatQuery ("CREATE DATABASE "%F.database) dbName))
  (manage q (formatQuery ("DROP DATABASE "%F.database) dbName))
  f
  where
    q = queryParams dbName
