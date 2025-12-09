module Composition where

{-@
cmp :: forall < p :: b -> c -> Bool
              , q :: a -> b -> Bool
              , r :: a -> c -> Bool
              >.
       {x::a, w::b<q x> |- c<p w> <: c<r x>}
       f:(y:b -> c<p y>)
    -> g:(z:a -> b<q z>)
    -> x:a -> c<r x>
@-}

cmp :: (b -> c)
    -> (a -> b)
    ->  a -> c

cmp f g x = f (g x)



{-@ incr :: x:Nat -> {v:Nat | v == x + 1} @-}
incr :: Int -> Int
incr x = x + 1


{-@ incr2 :: x:Nat -> {v:Nat | v = x + 2} @-}
incr2 :: Int -> Int
incr2 = incr `cmp` incr

