module Ch5 where

import Effect (Effect)
import Effect.Class.Console (log)
import Prelude (Unit, show, discard)
import Data.List (List(..), (:))

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
