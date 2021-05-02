{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Data.Typelevel.CFunction where

import Prelude hiding (not)
import Data.Kind
import Data.Proxy

-- The kind-level profunctor of relations
type a ⇒ b = a -> b -> Constraint

-- The result of evaluating a single type in the domain of a in a functional relation
type CCase :: a ⇒ b -> a ⇒ b
class (CFunction c, c a b) => CCase c a b | c a -> b

-- A functional relation (i.e. one for which the domain value uniquely determines the result over the entire domain)
type CFunction :: (a ⇒ b) -> Constraint
class (forall x y. c x y => CCase c x y) => CFunction c

-- The earthly entrance to a nethereal journey
type Dream :: (a ⇒ b) -> Type
data Dream c
  where
  Dream :: CFunction c => Dream c

-- Evaluation
(^$) :: c x y => Dream (c :: a ⇒ b) -> Proxy x -> Proxy y
(^$) Dream Proxy = Proxy

-- Composition
type (∘) :: (b ⇒ c) -> (a ⇒ b) -> a ⇒ c
class (f ∘ g) a b | f g a -> b

instance (CCase f x b, CCase g a x) => (f ∘ g) a b

-- Witness that composition forms a function
instance (f ∘ g) a b => CCase (f ∘ g) a b
instance CFunction (f ∘ g)

-- Earthly co-avatar of nethereal composition
(∘) :: (CFunction c, CFunction d) => Dream c -> Dream d -> Dream (c ∘ d)
Dream ∘ Dream = Dream

-- Test
type Not :: Bool ⇒ Bool
class Not a b | a -> b

-- We have to do this hemming and hawing for all functional relations
instance Not a b => CCase Not a b
instance CFunction Not

instance Not 'False 'True
instance Not 'True 'False

test1 :: Proxy _ -- GHC says "Found wildcard standing for 'True :: Bool"
test1 = Dream @Not ^$ Proxy @'False

test2 :: Proxy _ -- GHC says "Found wildcard standing for 'False :: Bool"
test2 = Dream @Not ∘ Dream @Not ^$ Proxy @'False
