{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Data.Typelevel.Function where

import Prelude hiding (not, id, (.))
import Data.Kind
import Data.Proxy
import Control.Category

-- A nethereal profunctor enriched in the category of types and relations
type a ⇒ b = a -> b -> Constraint

-- An earthly profunctor enriched in the category of types and functions
type a → b = a -> b -> Type

-- The result of evaluating a single type in the domain of a functional relation
type CCase :: a ⇒ b -> a ⇒ b
class (CFunction c, c a b) => CCase c a b | c a -> b

-- This, so we can write: `Not @ 'False := 'True`
type (^@) = CCase
infix 9 ^@
type (:=) fx = fx
infix 0 :=

-- A functional relation (i.e. one for which every type in the domain kind uniquely determines a result type)
type CFunction :: (a ⇒ b) -> Constraint
class (forall x y. c x y => c ^@ x := y) => CFunction c

-- The identity functional relation
type CId :: x ⇒ x
instance CId a a

-- NB: We have to do this hemming and hawing for all functional relations
class CId a b | a -> b
instance CId a b => CCase CId a b
instance CFunction CId

-- Composition
type (∘) :: (b ⇒ c) -> (a ⇒ b) -> a ⇒ c
instance (g ^@ a := x, f ^@ x := b) => (f ∘ g) a b

-- *cough, cough*
class (f ∘ g) a b | f g a -> b
instance (f ∘ g) a b => CCase (f ∘ g) a b
instance CFunction (f ∘ g)

-- A nethereal journey from point A to point B
type Dream :: a → b
data Dream a b
  where
  Dream :: (c ^@ a := b) => Proxy c -> Dream a b

dream :: forall c x y. (c ^@ x := y) => Dream x y
dream = Dream $ Proxy @c

instance Category Dream
  where
  id = dream @CId
  Dream (_ :: Proxy f) . Dream (_ :: Proxy g) = Dream $ Proxy @(f ∘ g)

-- Test
type Not :: Bool ⇒ Bool
instance Not 'False 'True
instance Not 'True 'False

-- *hack, hack*
class Not a b | a -> b
instance Not a b => CCase Not a b
instance CFunction Not

test1 :: Dream 'False _ -- GHC says "Found wildcard standing for 'True :: Bool"
test1 = dream @Not

test2 :: Dream 'False _ -- GHC says "Found wildcard standing for 'False :: Bool"
test2 = dream @Not . dream @Not
