module Main where

import Prelude

import Data.Array (sort)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.String as StringUnicode
import Data.String.CodePoints (CodePoint, codePointFromChar)
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "hello..."

type Address =
  { street1 :: String
  , street2 :: String
  , city :: String
  , state :: String
  , zip :: String
  }

newtype Person = Person
  { name :: String
  , age :: Int
  , address :: Address
  }

data Company = Company
  { name :: String
  , address :: Address
  }

data Residence
  = Home Address
  | Facility Address

class HasAddress a where
  getAddress :: a -> Address

-- < purs 14.2
-- instance hasAddressPerson :: HasAddress Person where
--   getAddress (Person p) = p.address

instance HasAddress Person where
  getAddress (Person p) = p.address

instance HasAddress Company where
  getAddress (Company c) = c.address

getDirections :: forall a. HasAddress a => a -> Address
getDirections = getAddress

instance Eq Person where
  eq (Person p1) (Person p2) =
    -- because p1/p2 are just records of things with Eq instances
    -- we can simple compare them, i.e this is unnecessary
    -- p1.name == p2.name
    --   && p1.age == p2.age
    --   && p1.address == p2.address
    p1 == p2

data Place = First | Second | Third

instance Show Place where
  show = case _ of
    First -> "first"
    Second -> "second"
    Third -> "third"

instance Eq Place where
  eq First First = true
  eq Second Second = true
  eq Third Third = true
  eq _ _ = false

instance Ord Place where
  compare First First = EQ
  compare Second Second = EQ
  compare Third Third = EQ
  compare First _ = GT
  compare Second First = LT
  compare Second _ = GT
  compare Third _ = LT

x_ :: Array Place
x_ = [ Third, First, Second ]

sx_ :: Array Place
sx_ = sort x_

data SomeType = This | That | TheOther | AndYetAnother

-- prior to 14.2, this would require a name
-- i.e. derive instance eqSomeType :: Eq SomeType
derive instance Eq SomeType
derive instance Ord SomeType
-- you can't write your own Generic instance, the '_'
-- is filled in by the compiler
derive instance Generic SomeType _

instance Show SomeType where
  show = genericShow

newtype FirstName = FirstName String

derive instance Newtype FirstName _

newtype LastName = LastName String

derive instance Newtype LastName _

fullName :: FirstName -> LastName -> String
fullName first last = unwrap first <> " " <> unwrap last

combineStringNewtypes
  :: forall a b
   . Newtype a String
  => Newtype b String
  => String
  -> a
  -> b
  -> String
combineStringNewtypes between n1 n2 =
  unwrap n1 <> between <> unwrap n2

lastNameFirst :: LastName -> FirstName -> String
lastNameFirst = combineStringNewtypes ", "

newtype Ceo = Ceo Person

derive instance Newtype Ceo _

newtype Janitor = Janitor Person

derive instance Newtype Janitor _

-- One option is to use this generic function to unwrap
-- and getAddress for the newtype,
--
-- @
-- genericPersonHasAddress :: forall a. Newtype a Person => a -> Address
-- genericPersonHasAddress = getAddress <<< unwrap
-- 
-- instance HasAddress Ceo where
--   getAddress = genericPersonHasAddress
-- 
-- instance HasAddress Janitor where
--   getAddress = genericPersonHasAddress
-- @
--
-- However, there is also newtype deriving!
derive newtype instance HasAddress Ceo
derive newtype instance HasAddress Janitor

-- class Combine a where
--   combine :: a -> a -> a
-- 
-- instance Combine Int where
--   combine = (+)
-- 
-- instance Combine Int where
--   combine = (*)
-- Instead, use newtypes for Sum/Product Semigroup
-- and implement them

class IsRecord a where
  isRecord :: a -> Boolean

instance IsRecord (Record a) where
  isRecord _ = true

-- If we don't get a record fall to the next, overlapping
-- instance, i.e. this one
else instance IsRecord a where
  isRecord _ = false

-- data Maybe a = Nothing | Just a
-- 
-- instance Show a => Show (Maybe a) where
--   show Nothing = "Nothing"
--   show (Just x) = "Just " <> show x

class Decapitate collection element where
  decapitate :: collection -> Maybe { head :: element, tail :: collection }

instance decapitateStringUnicode :: Decapitate String CodePoint where
  decapitate = StringUnicode.uncons

-- This is slightly unfortunate because we have to pass in a value
-- of `f element`, e.g. `Proxy :: Proxy a`
genericTail
  :: ∀ f collection element
   . Decapitate collection element
  => f element
  -> collection
  -> Maybe collection
genericTail _ xs =
  case (decapitate xs :: Maybe { head :: element, tail :: collection }) of
    Just { tail } -> Just tail
    Nothing -> Nothing

t :: Maybe String
t = genericTail (Identity (codePointFromChar 'a') :: Identity CodePoint) "abc"

class Behead collection element | element -> collection where
  behead :: collection -> Maybe { head :: element, tail :: collection }

genericTail'
  :: ∀ collection element
   . Behead collection element
  => collection
  -> Maybe collection
genericTail' xs =
  case (behead xs :: Maybe { head :: element, tail :: collection }) of
    Just { tail } -> Just tail
    Nothing -> Nothing

-- When using `Behead` we have two options,
-- - "forward", i.e. collection -> element
-- - "reverse", i.e. element -> collection
-- 
-- Forward fails with instances like `Behead String Char` and
-- `Behead String CodePoint` because we are saying the `collection`
-- i.e. `String` determines the element. However, an element in
-- a `String` can represent multiple things simultaneously.
-- This is less frequently an issue for containers and therefore
-- the forward style might be preferred. One could also use
-- newtypes to handle the `Char` versus `CodePoint` distinction.
