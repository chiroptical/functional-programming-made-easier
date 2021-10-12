module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

algebraProblem :: Int
algebraProblem =
  let
    s = 20

    b = 12

    r = 3
  in
    s - b - r

factorial :: Int -> Int
factorial 0 = 1

factorial n = n * factorial (n - 1)

add :: Int -> Int -> Int
add x y = x + y

add2 :: Int -> Int
add2 = add 2

main :: Effect Unit
main = do
  log "🍝"
