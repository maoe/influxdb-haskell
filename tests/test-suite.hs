{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
import Control.Applicative
import Control.Exception as E
import Control.Monad
import Control.Monad.Trans
import Data.Function
import Data.List (find)
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

case_post_with_precision :: Assertion
case_post_with_precision = runTest $ \config ->
  withTestDatabase config $ \database -> do
    name <- liftIO newName
    postWithPrecision config database SecondsPrecision $
      writeSeries name $ Val 42
    [series] <- query config database $
      "select value from " <> name
    fromSeriesData series @=? Right [Val 42]

case_listDatabases :: Assertion
case_listDatabases = runTest $ \config ->
  withTestDatabase config $ \name -> do
    databases <- listDatabases config
    assertBool ("No such database: " ++ T.unpack name) $
      any ((name ==) . databaseName) databases

case_create_then_drop_database :: Assertion
case_create_then_drop_database = runTest $ \config -> do
  name <- newName
  dropDatabaseIfExists config name
  createDatabase config name
  databases <- listDatabases config
  assertBool ("No such database: " ++ T.unpack name) $
    any ((name ==) . databaseName) databases
  dropDatabase config name
  databases' <- listDatabases config
  assertBool ("Found a dropped database: " ++ T.unpack name) $
    all ((name /=) . databaseName) databases'

case_list_cluster_admins :: Assertion
case_list_cluster_admins = runTest $ \config -> do
  admins <- listClusterAdmins config
  assertBool "No root admin" $
    any (("root" ==) . adminUsername) admins

case_add_then_delete_cluster_admin :: Assertion
case_add_then_delete_cluster_admin = runTest $ \config -> do
  name <- newName
  admin <- addClusterAdmin config name "somePassword"
  admins <- listClusterAdmins config
  assertBool ("No such admin: " ++ T.unpack name) $
    any ((name ==) . adminUsername) admins
  deleteClusterAdmin config admin
  admins' <- listClusterAdmins config
  assertBool ("Found a deleted admin: " ++ T.unpack name) $
    all ((name /=) . adminUsername) admins'

case_update_cluster_admin_password :: Assertion
case_update_cluster_admin_password = runTest $ \config -> do
  let curPassword = "somePassword"
      newPassword = "otherPassword"
  name <- newName
  deleteClusterAdminIfExists config name
  admin <- addClusterAdmin config name curPassword
  updateClusterAdminPassword config admin newPassword
  let newCreds = Credentials name newPassword
      newConfig = config { configCreds = newCreds }
  name <- newName
  dropDatabaseIfExists config name
  createDatabase newConfig name
  databases <- listDatabases newConfig
  assertBool ("No such database: " ++ T.unpack name) $
    any ((name ==) . databaseName) databases
  dropDatabase newConfig name
  databases' <- listDatabases newConfig
  assertBool ("Found a dropped database: " ++ T.unpack name) $
    all ((name /=) . databaseName) databases'

case_add_then_delete_database_users :: Assertion
case_add_then_delete_database_users = runTest $ \config ->
  withTestDatabase config $ \name -> do
    listDatabaseUsers config name >>= \users ->
      assertBool "There shouldn't be any users" $ null users
    newUserName <- newName
    addDatabaseUser config name newUserName "somePassword"
    listDatabaseUsers config name >>= \users ->
      assertBool ("No such user: " <> T.unpack newUserName) $
        any ((newUserName ==) . userName) users
    deleteDatabaseUser config name newUserName
    listDatabaseUsers config name >>= \users ->
      assertBool ("Found a deleted user: " <> T.unpack newUserName) $
        all ((newUserName /=) . userName) users

case_update_database_user_password :: Assertion
case_update_database_user_password = runTest $ \config ->
  withTestDatabase config $ \name -> do
    newUserName <- newName
    addDatabaseUser config name newUserName "somePassword"
    listDatabaseUsers config name >>= \users ->
      assertBool ("No such user: " <> T.unpack newUserName) $
        any ((newUserName ==) . userName) users
    updateDatabaseUserPassword config name newUserName "otherPassword"
    deleteDatabaseUser config name newUserName

case_grant_revoke_database_user :: Assertion
case_grant_revoke_database_user = runTest $ \config ->
  withTestDatabase config $ \name -> do
    newUserName <- newName
    addDatabaseUser config name newUserName "somePassword"
    listDatabaseUsers config name >>= \users ->
      assertBool ("No such user: " <> T.unpack newUserName) $
        any ((newUserName ==) . userName) users
    grantAdminPrivilegeTo config name newUserName
    listDatabaseUsers config name >>= \users -> do
      case find ((newUserName ==) . userName) users of
        Nothing -> assertFailure $ "No such user: " <> T.unpack newUserName
        Just user -> assertBool
          ("User is not privileged: " <> T.unpack newUserName)
          (userIsAdmin user)
    revokeAdminPrivilegeFrom config name newUserName
    listDatabaseUsers config name >>= \users -> do
      case find ((newUserName ==) . userName) users of
        Nothing -> assertFailure $ "No such user: " <> T.unpack newUserName
        Just user -> assertBool
          ("User is still privileged: " <> T.unpack newUserName)
          (not $ userIsAdmin user)
    deleteDatabaseUser config name newUserName

-------------------------------------------------

data Val = Val Int deriving (Eq, Show)

instance ToSeriesData Val where
  toSeriesColumns _ = V.fromList ["value"]
  toSeriesPoints (Val n) = V.fromList [toValue n]

instance FromSeriesData Val where
  parseSeriesData = withValues $ \values -> Val <$> values .: "value"

-------------------------------------------------

dropDatabaseIfExists :: Config -> Text -> IO ()
dropDatabaseIfExists config name =
  dropDatabase config name
    `catchAll` \_ -> return ()

deleteClusterAdminIfExists :: Config -> Text -> IO ()
deleteClusterAdminIfExists config name =
  deleteClusterAdmin config (Admin name)
    `catchAll` \_ -> return ()

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

withTestDatabase :: Config -> (Text -> IO a) -> IO a
withTestDatabase config = bracket acquire release
  where
    acquire = do
      name <- newName
      dropDatabaseIfExists config name
      createDatabase config name
      return name
    release = dropDatabase config

catchAll :: IO a -> (SomeException -> IO a) -> IO a
catchAll = E.catch
