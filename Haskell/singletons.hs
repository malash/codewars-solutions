{-# LANGUAGE NoImplicitPrelude, GADTs , DataKinds, TypeFamilies, TypeOperators, RankNTypes, DeriveFunctor #-}

module Singletons where

import Prelude hiding (drop, take, head, tail, index, zipWith, replicate, map, (++))

data Vec a n where
  VNil :: Vec a Zero
  VCons :: a -> Vec a n -> Vec a (Succ n)

-- promoted to type level by data kinds
data Nat = Zero | Succ Nat

data SNat a where
  SZero :: SNat Zero
  SSucc :: SNat a -> SNat (Succ a)

type family (a :: Nat) :< (b :: Nat) :: Bool
type instance m :< Zero = False
type instance Zero :< Succ n = True
type instance (Succ m) :< (Succ n) = m :< n

type family (Add (a :: Nat) (b :: Nat)) :: Nat
type instance Add Zero n = n
type instance Add (Succ m) n = Succ (Add m n)

type family (Dec0 (a :: Nat) (b :: Nat)) :: Nat
type instance Dec0 m Zero = m
type instance Dec0 Zero m = Zero
type instance Dec0 (Succ m) (Succ n) = Dec0 m n

type family (Min (a :: Nat) (b :: Nat)) :: Nat
type instance Min Zero m = Zero
type instance Min m Zero = Zero
type instance Min (Succ m) (Succ n) = Succ (Min m n)

map :: (a -> b) -> Vec a n -> Vec b n
map f VNil = VNil
map f (VCons x xs) = VCons (f x) (map f xs)

index :: ((a :< b) ~ True) => SNat a -> Vec s b -> s
index SZero (VCons v _) = v
index (SSucc n) (VCons _ xs) = index n xs

replicate :: s -> SNat a -> Vec s a
replicate x SZero = VNil
replicate x (SSucc n) = VCons x (replicate x n)

-- -- Both vectors must be of equal length
zipWith :: (a -> b -> c) -> Vec a n -> Vec b n -> Vec c n
zipWith _ VNil VNil =VNil
zipWith f (VCons x xs) (VCons y ys) = VCons (f x y) (zipWith f xs ys)

(++) :: Vec v m -> Vec v n -> Vec v (Add m n)
VNil ++ a = a
(VCons x xs) ++ a = VCons x (xs ++ a)

-- -- The semantics should match that of take for normal lists.
take :: SNat a -> Vec s b -> Vec s (Min a b)
take SZero _ = VNil
take _ VNil  = VNil
take (SSucc n) (VCons x xs) = VCons x (take n xs)

-- -- The semantics should match that of drop for normal lists.
drop :: SNat a -> Vec s b -> Vec s (Dec0 b a)
drop SZero x = x
drop _ VNil = VNil
drop (SSucc n) (VCons x xs) = drop n xs

head :: ((Zero :< m) ~ True) => Vec v m -> v
head (VCons x _) = x

tail :: (((Succ Zero) :< m) ~ True) => Vec v (Succ m) -> Vec v m
tail (VCons _ xs) = xs
