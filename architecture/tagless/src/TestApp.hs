module TestApp where

import Prelude hiding (get, gets, put, toString, fromString)
import Data.Bifunctor (first, second)
import Data.Functor.Identity ( Identity(runIdentity) )
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.UUID (UUID, fromString, nil, toString)
import Data.UUID.V4 qualified as UUID
import Repos
    ( CargoRegistry(..),
      CargoRegistryError(SomeCargoRegError),
      IdService(..),
      UserRegistry(..),
      UserRepoError(..)
     )
import System.IO.Unsafe (unsafePerformIO)
import Types ( Cargo(cGoods, cId)
  , CargoId(CargoId), Goods(Goods), Person(phone), User(User, getPerson) )
import UnliftIO (MonadUnliftIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (ask))

data TestAppState = TestAppState 
 { taCurrentUid :: UUID
 , taCargos :: Map CargoId Cargo
 , taUsers :: [User]
 }

newtype TestCargoApp a = TestCA
  { runTestCA :: ReaderT (IORef TestAppState) IO a
  }
  deriving newtype (Functor, Applicative, Monad, MonadReader (IORef TestAppState), MonadIO, MonadUnliftIO)

runTa :: TestCargoApp a -> IO a
runTa app = do
  state <- newIORef (TestAppState nil mempty mempty)
  runReaderT (runTestCA app) state
  

put :: (MonadReader (IORef a) m, MonadIO m) => a -> m ()
put s = do
  stateRef <- ask
  liftIO $ writeIORef stateRef s

get :: (MonadReader (IORef a) m, MonadIO m) => m a
get = ask >>= liftIO . readIORef

gets :: (MonadReader (IORef a) f, MonadIO f) => (a -> b) -> f b
gets f  = f <$> get

instance CargoRegistry TestCargoApp where
  addCargo c = do
    appS <- get
    case cGoods c of
      (Goods [x]) -> pure $ Left (SomeCargoRegError "Test error: registry unavailable")
      _ -> Right <$> put (appS {taCargos = Map.insert (cId c) c (taCargos appS)})

  allCargos =
    Right <$> gets (Map.elems . taCargos)

instance IdService TestCargoApp where
  nextCargoId = do
    appS <- get
    let lastUuid = taCurrentUid appS
        newLast =
          fromJust
            . fromString
            . (\(h : t) -> succ h : t)
            . toString
            $ lastUuid
    put appS {taCurrentUid = newLast}
    pure . Right $ CargoId newLast

instance UserRegistry TestCargoApp where
  addUser p = let user = User p in
    get >>= 
      \s -> put s{taUsers = user : taUsers s}
      >> pure (Right user)

  getUser ph = do
    users <- gets $ filter ((ph ==) . phone . getPerson) . taUsers
    case users of
      [u] -> pure $ Right u
      (_:_) -> pure $ Left (ManyUsersFound ph)
      [] -> pure $ Left (UserNotFound ph)

  allUsers = gets (Right . taUsers)