{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple"        @-}

module MA615ReflectAfterLet03 where

{-@ reflect g @-} 
g :: Int -> Int 
g x = x + 1

{-@ check :: x:Int -> {v:Int| v = (x + 1) } @-}
check :: Int -> Int
check x = g(x)

