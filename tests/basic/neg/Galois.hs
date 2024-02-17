module Boris where

{- import Language.Haskell.Liquid.Prelude -}

{-@ b2i :: b:Bool -> {i:Int | (i==0) <=> not b} @-}
b2i :: Bool -> Int
b2i b
  | b      = 1
  | not b  = 0

{-@ i2b :: i:Int -> {b:Bool | (i==0) <=> not b} @-}
i2b :: Int -> Bool
i2b i
  | (i==0)    = False
  | otherwise = True


{-@ i2i :: x:Int -> {y:Int | x==y} @-}
i2i :: Int -> Int
i2i x =
  b2i (i2b x)

