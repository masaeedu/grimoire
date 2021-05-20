{-# LANGUAGE ViewPatterns #-}
module Data.Typelevel.Fold where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import Data.Typelevel.Existential (Dict (..), Some (..))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Type.Reflection (TypeRep, Typeable, typeRep)
import Prelude

-- In Haskell, we have an earthly realm of types inhabited by terms, and a nethereal realm of kinds inhabited by types.

-- This module defines inter-universal folds which bridge the gap and transport nethereal entities to the earthly realm.

-- For now, the bridge only goes in one direction. Perhaps this is for the best.

-- | A passage through which to retrieve and embody nethereal entities
type Passage :: (k -> Constraint) -> Type -> Type
newtype Passage c x' = Passage {recite :: forall x. c x => Proxy x -> x'}
  deriving Functor

instance Applicative (Passage c)
  where
  pure a = Passage $ pure a
  Passage ff <*> Passage fa = Passage $ ff <*> fa

instance Monad (Passage c)
  where
  return = pure
  Passage ma >>= amb = Passage $ ma >>= (recite . amb)

passage :: forall c x'. (forall x. c x => Proxy x -> x') -> Passage c x'
passage = Passage

summon :: forall c. Passage c (Some (Dict c))
summon = Passage $ \(Proxy :: Proxy a) -> Some $ Dict @c @a

invoke :: Some (Dict c) -> Passage c r -> r
invoke (Some (Dict :: Dict c x)) p = recite p $ Proxy @x

inscribe :: Passage KnownSymbol String
inscribe = Passage symbolVal

type Embodiment k = Some (TypeRep @k)

embody :: Passage (Typeable @k) (Embodiment k)
embody = passage $ \(Proxy :: Proxy a) -> Some $ typeRep @a

-- | Some passages are recited over and over across many parts of a structure. We can use
--   an instance of @Rote@ to lift spells for summoning lesser entities to spells for summoning
--   a structured aggregate.
type Rote :: forall f k. (k -> Constraint) -> f k -> Constraint
class Rote c (v :: f k) where
  rote' ::
    -- | A nethereal passage for each type
    Passage c x ->
    -- | The nethereal data structure
    Proxy v ->
    -- | The earthly result
    f x

-- | Recite a passage by rote for each part of a composite structure
rote :: forall f c x. Passage c x -> Passage (Rote @f c) (f x)
rote p = Passage $ rote' p

-- | Like @Rote@, but for two type parameters
type Birote :: forall f k1 k2. (k1 -> Constraint) -> (k2 -> Constraint) -> f k1 k2 -> Constraint
class Birote c1 c2 (v :: f k1 k2) where
  birote' ::
    -- | A nethereal passage for types of the first kind
    Passage c1 x ->
    -- | A nethereal passage for types of the second kind
    Passage c2 y ->
    -- | The nethereal data structure
    Proxy v ->
    -- | The earthly result
    f x y

-- | Given two passages, recite the appropriate one for each part of a composite structure
birote :: forall f c1 c2 x y. Passage c1 x -> Passage c2 y -> Passage (Birote @f c1 c2) (f x y)
birote p1 p2 = Passage $ birote' p1 p2

-- Instances

-- Lists
instance Rote c '[] where
  rote' _ _ = []

instance (c x, Rote c xs) => Rote c (x ': xs) where
  rote' f _ = recite f (Proxy @x) : recite (rote f) (Proxy @xs)

-- Maybe
instance Rote c 'Nothing where
  rote' _ _ = Nothing

instance c a => Rote c ('Just a) where
  rote' f _ = Just $ recite f $ Proxy @a

-- Tuples
instance (c1 a, c2 b) => Birote c1 c2 '(a, b) where
  birote' fa fb _ = (recite fa $ Proxy @a, recite fb $ Proxy @b)

-- Either
instance c1 a => Birote c1 c2 ('Left a) where
  birote' fa _ _ = Left $ recite fa $ Proxy @a

instance c2 b => Birote c1 c2 ('Right b) where
  birote' _ fb _ = Right $ recite fb $ Proxy @b
