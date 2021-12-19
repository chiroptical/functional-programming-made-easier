module Main where

import Data.Array.NonEmpty (cons, singleton, (:))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Partial.Unsafe (unsafeCrashWith)
import Prelude (class Eq, class Show, Unit, mod, pure, unit, (+), bind, (<$>))
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (elements)

main :: Effect Unit
main = pure unit

class Semigroup a where
  append :: a -> a -> a

infixr 5 append as <>

class Semigroup a <= Monoid a where
  empty :: a

data AndBool = ABFalse | ABTrue

derive instance Eq AndBool
derive instance Generic AndBool _

instance Show AndBool where
  show = genericShow

instance Semigroup AndBool where
  append ABTrue ABTrue = ABTrue
  append _ _ = ABFalse

instance Monoid AndBool where
  empty = ABTrue

instance Arbitrary AndBool where
  arbitrary = elements (cons ABTrue (singleton ABFalse))

data OrBool = OBFalse | OBTrue

derive instance Eq OrBool
derive instance Generic OrBool _

instance Show OrBool where
  show = genericShow

instance Semigroup OrBool where
  append OBTrue _ = OBTrue
  append _ OBTrue = OBTrue
  append _ _ = OBFalse

instance Monoid OrBool where
  empty = OBFalse

instance Arbitrary OrBool where
  arbitrary = elements (cons OBTrue (singleton OBFalse))

data Mod4 = Zero | One | Two | Three

derive instance Eq Mod4
derive instance Generic Mod4 _

instance Show Mod4 where
  show = genericShow

toInteger :: Mod4 -> Int
toInteger = case _ of
  Zero -> 0
  One -> 1
  Two -> 2
  Three -> 3

-- The alternative is to just enumerate the cases...
instance Semigroup Mod4 where
  append x y =
    case (toInteger x + toInteger y) `mod` 4 of
      0 -> Zero
      1 -> One
      2 -> Two
      3 -> Three
      _ -> unsafeCrashWith "modulus 4 is broken?"

instance Monoid Mod4 where
  empty = Zero

instance Arbitrary Mod4 where
  arbitrary = elements (Zero : One : Two : (singleton Three))

class Monoid a <= Group a where
  inverse :: a -> a

instance Group Mod4 where
  inverse =
    case _ of
      Zero -> empty
      One -> Three
      Two -> Two
      Three -> One

data Maybe a = Nothing | Just a

derive instance Eq a => Eq (Maybe a)
derive instance Generic (Maybe a) _

instance Show a => Show (Maybe a) where
  show = genericShow

instance Semigroup a => Semigroup (Maybe a) where
  append Nothing x = x
  append x Nothing = x
  append (Just x) (Just y) = Just (x <> y)

instance Semigroup a => Monoid (Maybe a) where
  empty = Nothing

instance Arbitrary a => Arbitrary (Maybe a) where
  arbitrary = do
    val <- Just <$> arbitrary
    elements (Nothing : (singleton val))

newtype First a = First (Maybe a)

derive instance Eq a => Eq (First a)
derive instance Generic (First a) _

instance Show a => Show (First a) where
  show = genericShow

instance Semigroup a => Semigroup (First a) where
  append (First Nothing) x = x
  append x (First Nothing) = x
  append x@(First (Just _)) _ = x

instance Semigroup a => Monoid (First a) where
  empty = First Nothing

instance Arbitrary a => Arbitrary (First a) where
  arbitrary = First <$> arbitrary

newtype Last a = Last (Maybe a)

derive instance Eq a => Eq (Last a)
derive instance Generic (Last a) _

instance Show a => Show (Last a) where
  show = genericShow

instance Semigroup a => Semigroup (Last a) where
  append (Last Nothing) x = x
  append x (Last Nothing) = x
  append (Last (Just _)) y@(Last (Just _)) = y

instance Semigroup a => Monoid (Last a) where
  empty = Last Nothing

instance Arbitrary a => Arbitrary (Last a) where
  arbitrary = Last <$> arbitrary
