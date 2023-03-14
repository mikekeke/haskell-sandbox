{-# OPTIONS_GHC -Wno-orphans #-}

module App.UserRegistry where

import App.SQLiteInstances ()
import App.Types (App, AppEnv (dbConnPath))
import Control.Arrow (left)
import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Database.SQLite.Simple (Only (Only), SQLError, execute, query, query_, withConnection)
import Repos (UserRegistry (addUser, allUsers, getUser), UserRepoError (ConflictingUsers, ManyUsersFound, OtherUserRepoErr, UserNotFound))
import Types (Person, User, UserId, userFromPerson, userId)

instance MonadIO m => UserRegistry (App m) where
  addUser = addUser_

  getUser = getUser_

  allUsers = allUsers_

  {-# INLINE addUser #-}
  {-# INLINE allUsers #-}

addUser_ :: MonadIO m => Person -> App m (Either UserRepoError User)
addUser_ p = do
  let newUser = userFromPerson p
      uid = userId newUser
  existingUser <- getUser uid -- FIXME: transaction will be required here for other DBMSs
  case existingUser of
    Left (UserNotFound _) -> tryAdd newUser
    Right u | u /= newUser -> pure . Left $ ConflictingUsers uid u newUser -- FIXME: tests
    otherError -> pure otherError
  where
    tryAdd user = do
      bdPath <- asks dbConnPath
      liftIO $
        withConnection bdPath $ \conn -> do
          (left (OtherUserRepoErr . show) <$>) $
            try @SQLError $ do
              execute conn "INSERT OR IGNORE INTO user (id, name, phone) VALUES (?,?,?)" user
              pure user

allUsers_ :: MonadIO m => App m (Either UserRepoError [User])
allUsers_ = do
  path <- asks dbConnPath
  liftIO $
    withConnection path $ \conn ->
      tryQueryAll conn <&> left (OtherUserRepoErr . show)
  where
    tryQueryAll c =
      try @SQLError $
        (query_ c "SELECT * FROM user" :: IO [User])

getUser_ ::
  (MonadIO m, MonadReader AppEnv m) =>
  UserId ->
  m (Either UserRepoError User)
getUser_ uid = do
  path <- asks dbConnPath
  users <- liftIO $
    withConnection path $ \conn ->
      tryQueryAll conn <&> left (OtherUserRepoErr . show)
  pure $ case users of
    Right [u] -> Right u
    Right [] -> Left $ UserNotFound uid
    Right (_ : _ : _) -> Left $ ManyUsersFound uid
    Left sqlErr -> Left sqlErr
  where
    tryQueryAll c =
      try @SQLError $
        query c "SELECT * FROM user where id = ?" (Only uid)
