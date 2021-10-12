module Main where

import Prelude
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (error, log)

-- This does not strip leading whitespace
s3 :: String
s3 =
  """
  This is a multi-line that can contain quotes "" but \n will not be a newline
     hello...
  """

ex :: List Int
ex = 1 : 2 : 3 : Nil

r :: { firstName :: String, lastName :: String }
r = { firstName: "Joe", lastName: "Mama" }

type Person
  = { name :: String
    , age :: Int
    }

a :: Person
a =
  let
    person = { name: "Candy Cane", age: 37 }
  in
    person { name = "Randy Cane" }

data StringTriplet
  = StringTriplet String Int Int

data StringStats
  = StringStats
    { string :: String
    , length :: Int
    , vowelCount :: Int
    }

type Tuple'' a b
  = { fst :: a, snd :: b }

data Tuple' a b
  = Tuple' { fst :: a, snd :: b }

f :: StringTriplet -> StringStats
f (StringTriplet s l v) =
  StringStats
    { string: s
    , length: l
    , vowelCount: v
    }

g :: StringStats -> StringTriplet
g (StringStats { string, length, vowelCount }) = StringTriplet string length vowelCount

getFst :: forall a b. { fst :: a, snd :: b } -> a
getFst { fst } = fst

hush :: ∀ a b. Either a b -> Maybe b
hush (Left _) = Nothing

hush (Right x) = Just x

isNothing :: ∀ a. Maybe a -> Boolean
isNothing = case _ of
  Nothing -> true
  _ -> false

fromString :: String -> Maybe Boolean
fromString "true" = Just true

fromString "false" = Just false

fromString _ = Nothing

multTwo :: List Int -> Maybe Int
multTwo (x : y : _) = Just $ x * y

multTwo _ = Nothing

type Address
  = { street :: String
    , city :: String
    , state :: String
    , zip :: Int
    }

type Employee
  = { name :: String
    , jobTitle :: String
    , yearsAtCompany :: Int
    , address :: Address
    }

type Company
  = { name :: String
    , yearsInBusiness :: Int
    , address :: Address
    }

isCEO :: Employee -> Boolean
isCEO { jobTitle } = jobTitle == "CEO"

isCalifornia :: forall r. { address :: Address | r } -> Boolean
isCalifornia { address } = address.state == "CA"

data ContactMethod
  = Phone
  | Email
  | Fax

keepModern :: ContactMethod -> ContactMethod
keepModern = case _ of
  Phone -> Phone
  Email -> Email
  Fax -> Fax

-- Operator sections require underscores
f = (_ * 1)

-- Row polymorphic record updates
setAge :: forall r. Int -> { age :: Int | r } -> { age :: Int | r }
setAge n = _ { age = n }

multSum :: Int -> Int -> Tuple Int Int
multSum x y = Tuple mult sum
  where
  mult = x * y

  sum = x + y

multSum' :: Int -> Int -> Tuple Int Int
multSum' x y =
  let
    mult = x * y

    sum = x + y
  in
    Tuple mult sum

h :: forall a. a -> a
h = identity

main :: Effect Unit
main = do
  log s3
  log "Hello, world!"
