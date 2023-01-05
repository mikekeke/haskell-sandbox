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


---------------------
---- other tests ----
---------------------

data Fenotype
  = HasLegs
  | HasWings

type family CanFly (mode :: Fenotype) :: Bool where
  CanFly 'HasLegs = 'False
  CanFly 'HasWings = 'True


data Creature (ft :: Fenotype) where
  Bobr :: Creature 'HasLegs
  LeglessFlyingBobr :: Creature 'HasWings

{-
Ignore "Redundant constraint" warning.
W/o "CanFly fen ~ 'True" resNok will compile
-}
requestLandingZone ::
  CanFly fen ~ 'True =>
  Creature fen ->
  Maybe ()
requestLandingZone = error "todo"

-- resNok :: Maybe ()
-- resNok = requestLandingZone Bobr -- won't compile

resOk :: Maybe ()
resOk = requestLandingZone LeglessFlyingBobr