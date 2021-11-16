module Ch5 where

import Prelude hiding ((#), const, flip, ($), apply, append)
import Data.Tuple (Tuple(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log)

flip :: forall a b c. (a -> b -> c) -> b -> a -> c
flip f y x = f x y

-- \x -> \y -> f y x
const :: forall a b. a -> b -> a
const x _ = x

apply :: forall a b. (a -> b) -> a -> b
apply f x = f x

-- right associative
-- higher precedence comes first
infixr 0 apply as $

applyFlipped :: forall a b. a -> (a -> b) -> b
applyFlipped = flip apply

infixl 1 applyFlipped as #

-- Page 144, Section 5.10
test :: Effect Unit
test = do
  -- prints 2
  log $ show $ flip const 1 2
  -- alternatively `log (show (flip const 1 2))`
  flip const 1 2 # show # log

singleton :: forall a. a -> List a
singleton x = x : Nil

null :: forall a. List a -> Boolean
null = case _ of
  Nil -> true
  _ -> false

snoc :: forall a. a -> List a -> List a
snoc x = case _ of
  Nil -> x : Nil
  Cons y ys -> y : snoc x ys

length :: forall a. List a -> Int
length =
  let
    go :: Int -> List a -> Int
    go x = case _ of
      Nil -> x
      Cons _ ys -> go (x + 1) ys
  in
    go 0

length' :: forall a. List a -> Int
length' = case _ of
  Nil -> 0
  Cons _ xs -> 1 + length xs

head :: forall a. List a -> Maybe a
head = case _ of
  Nil -> Nothing
  Cons x _ -> Just x

tail :: forall a. List a -> Maybe (List a)
tail = case _ of
  Nil -> Nothing
  -- Shorthand for cons-list deconstruction
  (_ : xs) -> Just xs

last :: forall a. List a -> Maybe a
last = case _ of
  Nil -> Nothing
  (x : Nil) -> Just x
  (_ : xs) -> last xs

init :: forall a. List a -> Maybe (List a)
init =
  let
    go :: List a -> List a
    go = case _ of
      (_ : Nil) -> Nil
      (x : xs) -> x : go xs
      Nil -> Nil -- shouldn't be possible
  in
    case _ of
      Nil -> Nothing
      (_ : Nil) -> Just Nil
      xs -> Just $ go xs

uncons :: forall a. List a -> Maybe { head :: a, tail :: List a }
uncons = case _ of
  Nil -> Nothing
  (x : xs) -> Just { head: x, tail: xs }

index :: forall a. Int -> List a -> Maybe a
index n = case _ of
  _
    | n < 0 -> Nothing
  Nil -> Nothing
  (x : _)
    | n == 0 -> Just x
  (_ : xs) -> index (n - 1) xs

-- Technically this is backwards to the normal impl
infixl 8 index as !!

findIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findIndex p =
  let
    go acc = case _ of
      Nil -> Nothing
      (x : _)
        | p x -> Just acc
      (_ : xs) -> go (acc + 1) xs
  in
    go 0

findLastIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex p xs =
  let
    go acc@{ currentIdx } = case _ of
      Nil -> acc
      (y : ys)
        | p y -> go (acc { lastIdx = Just currentIdx }) ys
      (_ : ys) -> go (acc { currentIdx = currentIdx + 1 }) ys

    r = go { currentIdx: 0, lastIdx: Nothing } xs
  in
    r.lastIdx

reverse :: List ~> List -- equilivalent to 'forall a. List a -> List a'
reverse = case _ of
  Nil -> Nil
  (x : xs) -> snoc x (reverse xs)

reverse' :: List ~> List
reverse' =
  let
    go acc = case _ of
      Nil -> acc
      (x : xs) -> go (x : acc) xs
  in
    go Nil

append :: forall a. List a -> List a -> List a
append xs ys = case xs of
  Nil -> ys
  Cons z zs -> z : append zs ys

concat :: forall a. List (List a) -> List a
concat = case _ of
  Nil -> Nil
  (xs : xss) -> append xs (concat xss)

filter :: forall a. (a -> Boolean) -> List a -> List a
filter p = case _ of
  Nil -> Nil
  (x : xs)
    | p x -> x : filter p xs
  (_ : xs) -> filter p xs

catMaybes :: forall a. List (Maybe a) -> List a
catMaybes = case _ of
  Nil -> Nil
  (Nothing : xs) -> catMaybes xs
  (Just x : xs) -> x : catMaybes xs

range :: Int -> Int -> List Int
range x y
  | x == y = singleton y
  | x > y = x : range (x - 1) y
  | otherwise = x : range (x + 1) y

take :: forall a. Int -> List a -> List a
take n = case _ of
  Nil -> Nil
  _
    | n <= 0 -> Nil
  (x : xs) -> x : take (n - 1) xs

drop :: forall a. Int -> List a -> List a
drop n = case _ of
  Nil -> Nil
  (_ : xs)
    | n <= 0 -> xs
  (_ : xs) -> drop (n - 1) xs

takeWhile :: forall a. (a -> Boolean) -> List a -> List a
takeWhile p = case _ of
  (x : xs)
    | p x -> x : takeWhile p xs
  _ -> Nil

dropWhile :: forall a. (a -> Boolean) -> List a -> List a
dropWhile p = case _ of
  Nil -> Nil
  (x : xs)
    | p x -> dropWhile p xs
  xs -> xs

takeEnd :: forall a. Int -> List a -> List a
takeEnd n xs =
  let
    l = length xs
  in
    drop (l - n) xs

-- naive: 'reverse <<< take n <<< reverse'
dropEnd :: forall a. Int -> List a -> List a
dropEnd n xs =
  let
    l = length xs
  in
    take (l - n) xs

zip :: forall a b. List a -> List b -> List (Tuple a b)
zip Nil Nil = Nil

zip (_ : _) Nil = Nil

zip Nil (_ : _) = Nil

zip (x : xs) (y : ys) = Tuple x y : zip xs ys

unzip :: forall a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip =
  let
    go xs ys = case _ of
      Nil -> Tuple xs ys
      (Tuple x y : zs) -> go (x : xs) (y : ys) zs
  in
    go Nil Nil
