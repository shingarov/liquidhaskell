module L6maxint where

{-@ maxInt :: forall <p :: Int -> Bool>. 
           Int<p> -> Int<p> -> Int<p> @-}
maxInt :: Int -> Int -> Int
maxInt x y =
    let b = x < y in
    if b then x else x

{-@ test1 :: {v:Int | 0 < v} -> {v:Int | 0 < v} -> {v:Int | 0 < v} @-}
test1 :: Int -> Int -> Int
test1 x y = maxInt x y

{-@ test2 :: {v:Int | v < 0} -> {v:Int | v < 0} -> {v:Int | v < 0} @-}
test2 :: Int -> Int -> Int
test2 x y = maxInt x y

