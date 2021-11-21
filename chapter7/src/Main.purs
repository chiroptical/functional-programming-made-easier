module Main where

import Prelude

import Effect (Effect)
import Ch7a as Ch7a
import Ch7b as Ch7b

main :: Effect Unit
main = do
  Ch7a.test
  Ch7b.test
