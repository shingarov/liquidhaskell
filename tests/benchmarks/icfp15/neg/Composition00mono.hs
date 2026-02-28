module Composition where

{-@
cmp :: forall < p :: Int -> Int -> Bool
              , q :: Int -> Int -> Bool
              , r :: Int -> Int -> Bool
              >.
       {x::Int, w::Int<q x> |- Int<p w> <: Int<r x>}
       f:(y:Int -> Int<p y>)
    -> g:(z:Int -> Int<q z>)
    -> x:Int -> Int<r x>
@-}

cmp :: (Int -> Int)
    -> (Int -> Int)
    ->  Int -> Int

cmp f g x = f (g x + 1)
