{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
module Typelevel.CCC where

import Data.Kind (Constraint)

-- Hom
type (⇒) :: Constraint -> Constraint -> Constraint
type c ⇒ d = c => d
infixr 0 ⇒

-- Product
type (/\) :: Constraint -> Constraint -> Constraint
class (a, b)
  => a /\ b
instance (a, b) => a /\ b
infixr 6 /\

-- Unit
type (⊤) :: Constraint
class ()
  => (⊤)
instance (⊤)

-- Identity
class (⊤) ⇒ (x ⇒ x)
  => Identity x
instance Identity x

-- Composition
class ((b ⇒ c) /\ (a ⇒ b)) ⇒ (a ⇒ c)
  => Composition a b c
instance Composition a b c

-- Product bifunctor
class (a ⇒ b) /\ (c ⇒ d) ⇒ a /\ c ⇒ b /\ d
  => Bimap a b c d
instance Bimap a b c d

-- Product associator
class (x /\ y) /\ z ⇒ x /\ y /\ z
  => Associator1 x y z
instance Associator1 x y z

class x /\ y /\ z ⇒ (x /\ y) /\ z
  => Associator2 x y z
instance Associator2 x y z

-- Product left unitor
class x ⇒ (⊤) /\ x
  => LeftUnitor1 x
instance LeftUnitor1 x

class (⊤) /\ x ⇒ x
  => LeftUnitor2 x
instance LeftUnitor2 x

-- Product right unitor
class x ⇒ (⊤) /\ x
  => RightUnitor1 x
instance RightUnitor1 x

class (⊤) /\ x ⇒ x
  => RightUnitor2 x
instance RightUnitor2 x

-- Currying adjunction
class (x /\ y ⇒ z) ⇒ x ⇒ y ⇒ z
  => Curry x y z
instance Curry x y z

class (x ⇒ y ⇒ z) ⇒ (x /\ y) ⇒ z
  => Uncurry x y z
instance Uncurry x y z
