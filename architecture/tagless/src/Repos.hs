module Repos where


import Types ( CargoId, Cargo, Person, User, UserPhone, UserId )

-- Cargo repo --
data CargoRegistryError 
  = SomeCargoRegError Text
  deriving stock Show

class Monad m => CargoRegistry m where
  addCargo :: Cargo -> m (Either CargoRegistryError ())
  allCargos :: m (Either CargoRegistryError [Cargo])

-- ID service --
data IdServiceError
  = SomeIdServiceErr Text
  deriving stock Show

class Monad m => IdService m where
  nextCargoId :: m (Either IdServiceError CargoId)

-- Users repo --

data UserRepoError 
  = UserNotFound UserId
  | ManyUsersFound UserId
  | OtherUserRepoErr String
  deriving stock Show

class Monad m => UserRegistry m where
  addUser :: Person -> m (Either UserRepoError User)
  getUser :: UserId -> m (Either UserRepoError User)
  allUsers :: m (Either UserRepoError [User])
