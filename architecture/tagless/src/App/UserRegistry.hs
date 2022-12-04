{-# OPTIONS_GHC -Wno-orphans #-}
module App.UserRegistry where

import Control.Arrow (left)
import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Database.SQLite.Simple (SQLError, execute, query_)
import App.SQLiteInstances ()
import App.Types (App, AppEnv (dbConn))
import Repos (UserRegistry (addUser, allUsers, getUser), UserRepoError (OtherUserRepoErr, UserNotFound))
import Types (Person, User (User))

instance MonadIO m => UserRegistry (App m) where
  addUser = addUser_

  getUser _ = liftIO $ do
    putStrLn "Getting user"
    threadDelay 2_000_000
    pure $ Left (UserNotFound "test no found phone") -- FIXME

  allUsers = allUsers_

addUser_ :: MonadIO m => Person -> App m (Either UserRepoError User)
addUser_ p = do
  conn <- asks dbConn
  tryAdd conn
    <&> left (OtherUserRepoErr . show)
  where
    tryAdd conn =
      liftIO $
        try @SQLError $ do
          let user = User p
          execute conn "INSERT OR IGNORE INTO user (name, phone) VALUES (?,?)" user
          pure user

allUsers_ :: MonadIO m => App m (Either UserRepoError [User])
allUsers_ = do
  conn <- asks dbConn
  tryQueryAll conn
    <&> left (OtherUserRepoErr . show)
  where
    tryQueryAll c =
      liftIO . try @SQLError $
        (query_ c "SELECT * FROM user" :: IO [User])
