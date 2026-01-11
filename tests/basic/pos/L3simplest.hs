module L3simplest where

{-@ fortyTwo :: {v:Int | _  } @-}
fortyTwo :: Int
fortyTwo = 42

{-@ check :: {v:Int | v >= 0} @-}
check :: Int
check = fortyTwo

