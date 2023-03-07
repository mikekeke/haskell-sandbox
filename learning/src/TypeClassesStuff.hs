{-# LANGUAGE FunctionalDependencies #-}
--to see what type inference wil infer
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module TypeClassesStuff where

class Typer a b | a -> b

instance Typer Int Bool

instance
  (Typer a b, Typer c d) =>
  Typer (a, c) (b, d)

instance (Typer a b) => Typer [a] [b]

fn :: Typer a b => a -> b
fn = undefined

-- r1 :: Bool
r1 = fn (1 :: Int)

-- r2 :: (Bool, Bool)
r2 = fn ((1, 3) :: (Int, Int))

-- r3 :: (Bool, (Bool, Bool))
r3 = fn ((1, (2, 3)) :: (Int, (Int, Int)))

r4 = fn ((1, ([(1, 2), (2, 3)], 1)) :: (Int, ([(Int, Int)], Int)))
