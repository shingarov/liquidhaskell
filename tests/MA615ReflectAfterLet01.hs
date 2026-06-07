{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple"        @-}

module MA615ReflectAfterLet01 where

{-@ f :: x:Int -> {v:Int| v = x } @-} 
f :: Int -> Int 
f x = x

{-@ reflect g @-} 
g :: Int -> Int 
g x = x

{-@ check :: x:Int -> {v:Int| v = x } @-}
check :: Int -> Int
check x = f(g(x))

