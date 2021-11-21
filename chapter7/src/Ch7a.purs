module Ch7a where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

test :: Effect Unit
test = do
  -- Eq instances
  log <<< show $ Just 5 == Just 5
  log <<< show $ Just 5 == Just 2
  log <<< show $ Just 5 == Nothing
  log <<< show $ Nothing == Just 5
  log <<< show $ Nothing == (Nothing :: Maybe Unit)
  -- Ord instances
  log <<< show $ Just 1 < Just 5
  log <<< show $ Just 5 <= Just 5
  log <<< show $ Just 5 > Just 10
  log <<< show $ Just 10 >= Just 10
  log <<< show $ Just 99 > Nothing
  log <<< show $ Just 99 < Nothing

data Maybe a = Nothing | Just a

derive instance Eq a => Eq (Maybe a)
derive instance Ord a => Ord (Maybe a)
derive instance Generic (Maybe a) _

instance Show a => Show (Maybe a) where
  show = genericShow

-- Without the deriving clauses, this is the code we would need
-- to implement these instances...
-- 
-- instance Eq a => Eq (Maybe a) where
--   eq Nothing Nothing = true
--   eq (Just a) (Just b) =  a == b
--   eq _ _ = false
-- 
-- instance Ord a => Ord (Maybe a) where
--   compare Nothing Nothing = EQ
--   compare (Just a) (Just b) = compare a b
--   compare Nothing (Just _) = LT
--   compare (Just _) Nothing = GT
--
-- instance Show a => Show (Maybe a) where
--   show =
--     case _ of
--       Nothing -> "Nothing"
--       Just a -> "Just " <> show a

greaterThanOrEq' :: forall a. Ord a => a -> a -> Boolean
greaterThanOrEq' x y =
  case compare x y of
    GT -> true
    EQ -> true
    _ -> false

data Either a b = Left a | Right b

derive instance (Eq a, Eq b) => Eq (Either a b)
derive instance (Ord a, Ord b) => Ord (Either a b)
derive instance Generic (Either a b) _

instance (Show a, Show b) => Show (Either a b) where
  show = genericShow
