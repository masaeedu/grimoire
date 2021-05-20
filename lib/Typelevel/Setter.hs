module Typelevel.Setter where

import Typelevel.Function

type Setter s t a b = (a ⇒ b) -> s ⇒ t

type CFunctor :: Setter (f a) (f b) a b
class CFunctor c fa fb

instance CFunctor c 'Nothing 'Nothing
instance c a b => CFunctor c ('Just a) ('Just b)

instance CFunctor c '[] '[]
instance c x y => CFunctor c (x ': xs) (y ': ys)
