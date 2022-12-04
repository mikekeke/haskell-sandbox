{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Control.Concurrent (Chan, newChan, threadDelay, writeChan, dupChan, readChan, forkIO)
import Control.Concurrent.Async (poll, withAsync)
import System.IO (getChar)
import Data.Text qualified as T
import Data.Typeable (cast)

main :: IO ()
main = do
  print cbs1
  print $ (cast (T A) :: Maybe T)
  print $ (cast (T A) :: Maybe A)
  print $ (castAa (T A) :: Maybe A)
  print $ (castAa (T B) :: Maybe A)


data T where
  T :: (Show a, Typeable a) => a -> T

deriving instance Show T

data A = A deriving stock Show
data B = B deriving stock Show

castAa :: Typeable b => T -> Maybe b
castAa (T v) = cast v

cbs1 = [T A, T B]




--------------------------
newtype Rt r m a = Rt {rr :: r -> m a}

instance Functor (Rt r m) where

instance Applicative (Rt r m) where

instance Monad m => Monad (Rt r m) where
  return a = Rt $ \_ -> return a

  rt >>= k = Rt $ \r -> rr rt r >>= \v -> rr (k v) r