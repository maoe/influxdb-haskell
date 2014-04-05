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
case_post = runTest $ \config manager ->
  withTestDatabase config manager $ \database -> do
    name <- liftIO newName
    post config manager database $
      writeSeries name $ Val 42
    [series] <- query config manager database $
      "select value from " <> name
    fromSeriesData series @=? Right [Val 42]

case_post_multi_series :: Assertion
case_post_multi_series = runTest $ \config manager ->
  withTestDatabase config manager $ \database -> do
    name <- liftIO newName
    post config manager database $ do
      writeSeries name $ Val 42
      writeSeries name $ Val 42
      writeSeries name $ Val 42
    [series] <- query config manager database $
      "select value from " <> name
    fromSeriesData series @=? Right [Val 42, Val 42, Val 42]

case_post_multi_points :: Assertion
case_post_multi_points = runTest $ \config manager ->
  withTestDatabase config manager $ \database -> do
    name <- liftIO newName
    post config manager database $ withSeries name $ do
      writePoints $ Val 42
      writePoints $ Val 42
      writePoints $ Val 42
    [series] <- query config manager database $
      "select value from " <> name
    fromSeriesData series @=? Right [Val 42, Val 42, Val 42]

case_postWithPrecision :: Assertion
case_postWithPrecision = runTest $ \config manager ->
  withTestDatabase config manager $ \database -> do
    name <- liftIO newName
    postWithPrecision config manager database SecondsPrecision $
      writeSeries name $ Val 42
    [series] <- query config manager database $
      "select value from " <> name
    fromSeriesData series @=? Right [Val 42]

case_listDatabases :: Assertion
case_listDatabases = runTest $ \config manager ->
  withTestDatabase config manager $ \(Database name _) -> do
    databases <- listDatabases config manager
    assertBool ("No such database: " ++ T.unpack name) $
      any ((name ==) . databaseName) databases

-------------------------------------------------

data Val = Val Int deriving (Eq, Show)

instance ToSeriesData Val where
  toSeriesColumns _ = V.fromList ["value"]
  toSeriesPoints (Val n) = V.fromList [toValue n]

instance FromSeriesData Val where
  parseSeriesData = withValues $ \values -> Val <$> values .: "value"

-------------------------------------------------

runTest :: (Config -> HC.Manager -> IO a) -> IO a
runTest f = do
  pool <- newServerPool localServer []
  let config = Config rootCreds pool
  HC.withManager settings (f config)
  where
    settings = HC.defaultManagerSettings

newName :: IO Text
newName = do
  uniq <- newUnique
  return $ T.pack $ "test_" ++ show (hashUnique uniq)

withTestDatabase :: Config -> HC.Manager -> (Database -> IO a) -> IO a
withTestDatabase config manager = bracket acquire release
  where
    acquire = do
      name <- newName
      void (dropDatabase config manager (Database name Nothing))
        `catchAll` \_ -> return ()
      createDatabase config manager name
    release = dropDatabase config manager

catchAll :: IO a -> (SomeException -> IO a) -> IO a
catchAll = E.catch
