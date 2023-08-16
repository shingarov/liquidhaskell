{-# LANGUAGE FlexibleContexts #-}

module Find where

import Prelude
import Language.Haskell.Liquid.Prelude

{-@ LIQUID "--no-termination" @-}

{-@ find :: forall <p :: Int -> Bool>.
            {x :: Int <p> |- {v:Int | v == x + 1} <: Int <p> }
            (Int -> Bool) -> (Int<p> -> a) -> Int<p> -> a @-}
find :: (Int -> Bool) -> (Int -> a) -> Int -> a
find q k i | q i       = k i
           | otherwise = find q k (i + 1)
