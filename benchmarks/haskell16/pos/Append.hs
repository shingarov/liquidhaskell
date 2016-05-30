{-@ LIQUID "--higherorder"     @-}
{-@ LIQUID "--totality"        @-}
{-@ LIQUID "--exact-data-cons" @-}
{-@ LIQUID "--higherorderqs" @-}


{-# LANGUAGE IncoherentInstances   #-}
{-# LANGUAGE FlexibleContexts #-}
module MapFusion where

import Prelude hiding (map, concatMap)

import Proves


{-@ axiomatize append @-}
append :: L a -> L a -> L a
append xs ys
  | llen xs == 0 = ys
  | otherwise    = hd xs ::: append (tl xs) ys

{-@ axiomatize map @-}
map :: (a -> b) -> L a -> L b
map f xs
  | llen xs == 0 = N
  | otherwise    = f (hd xs) ::: map f (tl xs)

{-@ axiomatize concatMap @-}
concatMap :: (a -> L b) -> L a -> L b
concatMap f xs
  | llen xs == 0 = N
  | otherwise    = append (f (hd xs)) (concatMap f (tl xs))


{-@ axiomatize concatt @-}
concatt :: L (L a) -> L a
concatt xs
  | llen xs == 0 = N
  | otherwise    = append (hd xs) (concatt (tl xs))


prop_append_neutral :: L a -> Proof
{-@ prop_append_neutral :: xs:L a -> {append xs N == xs}  @-}
prop_append_neutral N
  = append N N ==! N
  *** QED
prop_append_neutral (x ::: xs)
  = append (x ::: xs) N
  ==! x ::: (append xs N)
  ==! x ::: xs             ? prop_append_neutral xs
  *** QED

{-@ prop_assoc :: xs:L a -> ys:L a -> zs:L a
               -> {append (append xs ys) zs == append xs (append ys zs) } @-}
prop_assoc :: L a -> L a -> L a -> Proof
prop_assoc N ys zs
  =   append (append N ys) zs
  ==! append ys zs
  ==! append N (append ys zs)
  *** QED

prop_assoc (x ::: xs) ys zs
  =   append (append (x ::: xs) ys) zs
  ==! append (x ::: append xs ys) zs
  ==! x ::: append (append xs ys) zs
  ==! x ::: append xs (append ys zs)  ? prop_assoc xs ys zs
  ==! append (x ::: xs) (append ys zs)
  *** QED



{-@ prop_map_append ::  f:(a -> a) -> xs:L a -> ys:L a
                    -> {map f (append xs ys) == append (map f xs) (map f ys) }
  @-}
prop_map_append :: (a -> a) -> L a -> L a -> Proof
prop_map_append f N ys
  =   map f (append N ys)
  ==! map f ys
  ==! append N (map f ys)
  ==! append (map f N) (map f ys)
  *** QED

prop_map_append f (x ::: xs) ys
  =   map f (append (x ::: xs) ys)
  ==! map f (x ::: append xs ys)
  ==! f x ::: map f (append xs ys)
  ==! f x ::: append (map f xs) (map f ys) ? prop_map_append f xs ys
  ==! append (f x ::: map f xs) (map f ys)
  ==! append (map f (x ::: xs)) (map f ys)
  *** QED


{-@ prop_concatMap :: f:(a -> L (L a)) -> xs:L a
                   -> { concatt (map f xs) == concatMap f xs }
  @-}

prop_concatMap :: (a -> L (L a)) -> L a -> Proof
prop_concatMap f N
  =   concatt (map f N)
  ==! concatt N
  ==! N
  ==! concatMap f N
  *** QED

prop_concatMap f (x ::: xs)
  =   concatt (map f (x ::: xs))
  ==! concatt (f x ::: map f xs)
  ==! append (f x) (concatt (map f xs))
  ==! append (f x) (concatMap f xs)     ? prop_concatMap f xs
  ==! concatMap f (x ::: xs)
  *** QED



data L a = N | a ::: (L a)
{-@ data L [llen] @-}


{-@ measure llen @-}
llen :: L a -> Int
{-@ llen :: L a -> Nat @-}
llen N        = 0
llen (_ ::: xs) = 1 + llen xs

{-@ measure hd @-}
{-@ hd :: {v:L a | llen v > 0 } -> a @-}
hd :: L a -> a
hd (x ::: _) = x

{-@ measure tl @-}
{-@ tl :: xs:{L a | llen xs > 0 } -> {v:L a | llen v == llen xs - 1 } @-}
tl :: L a -> L a
tl (_ ::: xs) = xs
