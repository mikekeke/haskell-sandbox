{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module TypeFamilies where

-- https://serokell.io/blog/type-families-haskell

-- Closed families --

type Not :: Bool -> Bool
type family Not a where
  Not True = False
  Not False = True

type FromMaybe :: a -> Maybe a -> a
type family FromMaybe d x where
  FromMaybe d Nothing = d
  FromMaybe _ (Just x) = x

type Fst :: (a, b) -> a
type family Fst t where
  Fst '(x, _) = x
