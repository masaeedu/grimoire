{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Typelevel.Existential where

import Data.Functor.Product (Product (Pair))
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import Type.Reflection (TypeRep, Typeable, eqTypeRep, typeRep, withTypeable, (:~~:) (HRefl))
import Prelude

type KindOf (x :: k) = k

-- Some
type Some :: (k -> Type) -> Type
data Some f where
  Some :: f a -> Some f

deriving instance (forall x. Show (f x)) => Show (Some f)

instance (Implies Typeable f, forall x. (Eq (f x))) => Eq (Some f) where
  Some (a :: f a) == Some (b :: f b) = case (imply @Typeable a, imply @Typeable b) of
    (Dict, Dict) -> case typeRep @a `eqTypeRep` typeRep @b of
      Nothing -> False
      Just HRefl -> a == b

someDict :: forall c x. c x => Some (Dict c)
someDict = Some $ Dict @c @x

someTypeRep :: forall x. Typeable x => Some (TypeRep @(KindOf x))
someTypeRep = Some $ typeRep @x

mapSome :: (forall x. f x -> g x) -> Some f -> Some g
mapSome f (Some v) = Some $ f v

-- Dict
type Dict :: (k -> Constraint) -> k -> Type
data Dict c a where
  Dict :: c a => Dict c a

type SomeDict c = Some (Dict c)

-- Patterns
pattern SomeDict :: forall c. () => forall x. c x => Proxy x -> Some (Dict c)
pattern SomeDict p <- (mapSome (`Pair` Proxy) -> Some (Pair Dict p))

-- Implication
type Implies :: (k -> Constraint) -> (k -> Type) -> Constraint
class Implies c f where
  imply' :: forall x. f x -> Dict c x

imply :: forall c f x. Implies c f => f x -> Dict c x
imply = imply'

instance Implies c (Dict c) where
  imply' = id

instance Implies Typeable TypeRep where
  imply' tr = withTypeable tr Dict
