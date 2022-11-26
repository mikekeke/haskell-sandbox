module IOApp.UserRegistry where

import Control.Arrow (left)
import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Functor ((<&>))
import Database.SQLite.Simple (SQLError, execute, query_)
import IOApp.SQLiteInstances
import IOApp.Types (IOApp, IOAppEnv (dbConn))
import Repos (UserRegistry (addUser, allUsers, getUser), UserRepoError (OtherUSerRepoErr, UserNotFound))
import Types (Person, User (User))

instance UserRegistry IOApp where
  addUser = addUser_

  getUser _ = liftIO $ do
    putStrLn "Getting user"
    threadDelay 2_000_000
    pure $ Left (UserNotFound "test no found phone") -- FIXME

  allUsers = allUsers_

addUser_ :: Person -> IOApp (Either UserRepoError User)
addUser_ p = do
  conn <- asks dbConn
  tryAdd conn
    <&> left (OtherUSerRepoErr . show)
  where
    tryAdd conn =
      liftIO $
        try @SQLError $ do
          let user = User p
          execute conn "INSERT INTO user (name, phone) VALUES (?,?)" user
          pure user

allUsers_ :: IOApp (Either UserRepoError [User])
allUsers_ = do
  conn <- asks dbConn
  tryQueryAll conn
    <&> left (OtherUSerRepoErr . show)
  where
    tryQueryAll c =
      liftIO . try @SQLError $
        (query_ c "SELECT * FROM user" :: IO [User])
