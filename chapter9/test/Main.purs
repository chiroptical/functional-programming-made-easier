module Test.Main where

import Effect (Effect)
import Main (class Group, class Monoid, class Semigroup, AndBool, Maybe, Mod4, OrBool, empty, inverse, (<>), First, Last)
import Prelude (Unit, (==), class Eq, (&&), discard)
import Test.QuickCheck (quickCheck)

propSemigroup :: forall a. Eq a => Semigroup a => a -> a -> a -> Boolean
propSemigroup a b c = (a <> b) <> c == a <> (b <> c)

propMonoid :: forall a. Eq a => Monoid a => a -> Boolean
propMonoid a = (empty <> a == a) && (a <> empty == a)

propGroup :: forall a. Eq a => Group a => a -> Boolean
propGroup a = a <> inverse a == empty

propCommutative :: forall a. Eq a => Group a => a -> a -> Boolean
propCommutative a b = a <> b == b <> a

main :: Effect Unit
main = do
  quickCheck \(a :: AndBool) b c -> propSemigroup a b c
  quickCheck \(a :: AndBool) -> propMonoid a
  quickCheck \(a :: OrBool) b c -> propSemigroup a b c
  quickCheck \(a :: OrBool) -> propMonoid a
  quickCheck \(a :: Mod4) b c -> propSemigroup a b c
  quickCheck \(a :: Mod4) -> propMonoid a
  quickCheck \(a :: Mod4) -> propGroup a
  quickCheck \(a :: Mod4) b -> propCommutative a b
  quickCheck \(a :: Maybe AndBool) b c -> propSemigroup a b c
  quickCheck \(a :: Maybe AndBool) -> propMonoid a
  quickCheck \(a :: First AndBool) b c -> propSemigroup a b c
  quickCheck \(a :: First AndBool) -> propMonoid a
  quickCheck \(a :: Last AndBool) b c -> propSemigroup a b c
  quickCheck \(a :: Last AndBool) -> propMonoid a
