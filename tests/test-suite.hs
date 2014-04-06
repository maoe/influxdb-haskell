{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
import Control.Applicative
import Control.Exception as E
import Control.Monad
import Control.Monad.Trans
import Data.Function
import Data.Monoid
import Data.Text (Text)
import Data.Unique
import qualified Data.Text as T
import qualified Data.Vector as V

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import qualified Network.HTTP.Client as HC

import Database.InfluxDB

main :: IO ()
main = $defaultMainGenerator

case_post :: Assertion
case_post = runTest $ \config ->
  withTestDatabase config $ \database -> do
    name <- liftIO newName
    post config database $
      writeSeries name $ Val 42
    [series] <- query config database $
      "select value from " <> name
    fromSeriesData series @=? Right [Val 42]

case_post_multi_series :: Assertion
case_post_multi_series = runTest $ \config ->
  withTestDatabase config $ \database -> do
    name <- liftIO newName
    post config database $ do
      writeSeries name $ Val 42
      writeSeries name $ Val 42
      writeSeries name $ Val 42
    [series] <- query config database $
      "select value from " <> name
    fromSeriesData series @=? Right [Val 42, Val 42, Val 42]

case_post_multi_points :: Assertion
case_post_multi_points = runTest $ \config ->
  withTestDatabase config $ \database -> do
    name <- liftIO newName
    post config database $ withSeries name $ do
      writePoints $ Val 42
      writePoints $ Val 42
      writePoints $ Val 42
    [series] <- query config database $
      "select value from " <> name
    fromSeriesData series @=? Right [Val 42, Val 42, Val 42]

case_postWithPrecision :: Assertion
case_postWithPrecision = runTest $ \config ->
  withTestDatabase config $ \database -> do
    name <- liftIO newName
    postWithPrecision config database SecondsPrecision $
      writeSeries name $ Val 42
    [series] <- query config database $
      "select value from " <> name
    fromSeriesData series @=? Right [Val 42]

case_listDatabases :: Assertion
case_listDatabases = runTest $ \config ->
  withTestDatabase config $ \(Database name _) -> do
    databases <- listDatabases config
    assertBool ("No such database: " ++ T.unpack name) $
      any ((name ==) . databaseName) databases

case_listClusterAdmins :: Assertion
case_listClusterAdmins = runTest $ \config -> do
  admins <- listClusterAdmins config
  assertBool "No root admin" $
    any (("root" ==) . adminUsername) admins

case_add_then_delete_cluster_admins :: Assertion
case_add_then_delete_cluster_admins = runTest $ \config -> do
  name <- newName
  admin <- addClusterAdmin config name
  admins <- listClusterAdmins config
  assertBool ("No such admin: " ++ T.unpack name) $
    any ((name ==) . adminUsername) admins
  deleteClusterAdmin config admin
  admins' <- listClusterAdmins config
  assertBool ("Found a deleted admin: " ++ T.unpack name) $
    all ((name /=) . adminUsername) admins'

-------------------------------------------------

data Val = Val Int deriving (Eq, Show)

instance ToSeriesData Val where
  toSeriesColumns _ = V.fromList ["value"]
  toSeriesPoints (Val n) = V.fromList [toValue n]

instance FromSeriesData Val where
  parseSeriesData = withValues $ \values -> Val <$> values .: "value"

-------------------------------------------------

runTest :: (Config -> IO a) -> IO a
runTest f = do
  pool <- newServerPool localServer []
  HC.withManager settings (f . Config rootCreds pool)
  where
    settings = HC.defaultManagerSettings

newName :: IO Text
newName = do
  uniq <- newUnique
  return $ T.pack $ "test_" ++ show (hashUnique uniq)

withTestDatabase :: Config -> (Database -> IO a) -> IO a
withTestDatabase config = bracket acquire release
  where
    acquire = do
      name <- newName
      void (dropDatabase config (Database name Nothing))
        `catchAll` \_ -> return ()
      createDatabase config name
    release = dropDatabase config

catchAll :: IO a -> (SomeException -> IO a) -> IO a
catchAll = E.catch
