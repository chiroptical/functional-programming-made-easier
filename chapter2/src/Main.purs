module Main where

import Prelude
import Data.String.CodePoints (length)
import Data.String.Common (toLower, toUpper)
import Effect (Effect)
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Exception (throw)

error :: forall a. String -> a
error = unsafePerformEffect <<< throw

isSmall :: String -> Boolean
isSmall s = length s < 10

isOddLength :: String -> Boolean
isOddLength s = length s `mod` 2 /= 0

appendIf :: (String -> Boolean) -> String -> String -> String
appendIf pred s append = if pred s then s <> append else s

isEven :: Int -> Boolean
isEven n = n `mod` 2 == 0

upperLower :: Int -> (String -> String)
upperLower n = if isEven n then toUpper else toLower

isLarge :: Int -> Boolean
isLarge n = n > 1000

notLarge :: Int -> Boolean
notLarge n = not <<< isLarge $ n

padLeft :: Char -> Int -> String -> String
padLeft c size s = error "Not implemented yet..."

zeroPad :: Int -> String -> String
zeroPad = padLeft '0'

main :: Effect Unit
main = do
  log $ appendIf isSmall "Hello World" "!!!"
  log $ appendIf isOddLength "Hello World" "!!!"
  log $ upperLower 0 "this should be output in uppercase"
  log $ upperLower 1 "THIS SHOULD BE OUTPUT IN LOWERCASE"
