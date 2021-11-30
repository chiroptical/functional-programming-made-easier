module Main where

import Prelude

import Data.Foldable (class Foldable, foldr)
import Data.Newtype (class Newtype)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Ring (class Ring)
import Effect (Effect)

combine :: forall f a. Foldable f => Monoid a => f a -> a
combine = foldr (<>) mempty

combine' :: forall f a. Foldable f => Semigroup a => NonEmpty f a -> a
combine' (x :| xs) = foldr (<>) x xs

class Monoid g <= Group g where
  ginverse :: g -> g

newtype MyInt = MyInt Int

derive instance Newtype MyInt _
derive newtype instance Semiring MyInt
derive newtype instance Ring MyInt

instance Semigroup MyInt where
  append (MyInt a) (MyInt b) = MyInt $ a + b

instance Monoid MyInt where
  mempty = MyInt 0

class Monoid g <= Group' g where
  ginverse' :: g -> g

instance Group' MyInt where
  ginverse' = negate

propGroup :: forall a. Eq a => Group' a => a -> Boolean
propGroup x = ginverse' x <> x == mempty

class Semigroup g <= Commutative g

-- Alternate implementation for Abelian
-- with a single instance for every type.
-- This is modeled after 'Field', i.e.
-- https://github.com/purescript/purescript-prelude/blob/68f8012bc2309d9bf5832cdf7316ad052d586905/src/Data/Field.purs#L39-L41
class (Commutative a, Group a) <= Abelian' a

instance (Commutative a, Group a) => Abelian' a

-- Could use a combination of
-- - https://pursuit.purescript.org/packages/purescript-quickcheck
-- - https://pursuit.purescript.org/packages/purescript-quickcheck-laws
-- to test various abstract algebra properties for types

main :: Effect Unit
main = pure unit
