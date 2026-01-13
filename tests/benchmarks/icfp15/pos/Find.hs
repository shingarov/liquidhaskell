module Find where

{-@ find :: forall <p :: Int -> Bool>.
            {x :: Int <p> |- {v:Int | v == x + 1} <: Int <p> }
            (Int -> Bool) -> (Int<p> -> ()) -> Int<p> -> () @-}
find :: (Int -> Bool) -> (Int -> ()) -> Int -> ()
find q k i | q i       = k i
           | otherwise = find q k (i + 1)

