module ICFP15 where

import Prelude hiding ((.), (++),  filter)

import Language.Haskell.Liquid.Prelude (unsafeError)

{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--no-totality" @-}
{-@ LIQUID "--short-names" @-}

{-@
(.) :: forall <p :: b -> c -> Bool, q :: a -> b -> Bool, r :: a -> c -> Bool>.
       {x::a, w::b<q x> |- c<p w> <: c<r x>}
       (y:b -> c<p y>)
    -> (z:a -> b<q z>)
    ->  x:a -> c<r x>
@-}
(.) f g x = f (g x)

-- Usage

{-@ plusminus :: n:Nat -> m:Nat -> x:{Nat | x <= m} -> {v:Nat | v = (m - x) + n} @-}
plusminus :: Int -> Int -> Int -> Int
plusminus n m = (n+) . (m-)

-- Qualifiers

{- qualif PLUSMINUS(v:int, x:int, y:int, z:int): (v = (x - y) + z) @-}
{- qualif PLUS     (v:int, x:int, y:int)       : (v = x + y)       @-}
{- qualif MINUS    (v:int, x:int, y:int)       : (v = x - y)       @-}


-- Folding
----------
-- see `FoldAbs.hs`

-- Appending Sorted Lists
-------------------------
{-@ type OList a = [a]<{\x v -> v >= x}> @-}

{-@ (++) :: forall <p :: a -> Bool, q :: a -> Bool, w :: a -> a -> Bool>.
        {x::a<p> |- a<q> <: a<w x>}
        [a<p>]<w> -> [a<q>]<w> -> [a]<w> @-}
(++) []      ys = ys
(++) (x:xs) ys = x:(xs ++ ys)

