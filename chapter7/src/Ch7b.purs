module Ch7b where

import Prelude

import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap, unwrap)
import Effect (Effect)
import Effect.Console (log)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..))

test :: Effect Unit
test = do
  log <<< show $
    Person
      { name: wrap "chiro"
      , age: wrap 30
      , occupation: Doctor
      }

newtype CSV = CSV String

derive instance Newtype CSV _

class ToCSV a where
  toCSV :: a -> CSV

genericToCSV
  :: forall a
   . Show a
  => a
  -> CSV
genericToCSV = CSV <<< show

newtype FullName = FullName String

derive instance Newtype FullName _
derive newtype instance Show FullName

newtype Age = Age Int

derive instance Newtype Age _
derive newtype instance Show Age

data Occupation = Doctor | Dentist | Lawyer | Unemployed

derive instance Generic Occupation _
instance Show Occupation where
  show = genericShow

occupationFromString :: String -> Maybe Occupation
occupationFromString = case _ of
  "Doctor" -> Just Doctor
  "Dentist" -> Just Dentist
  "Lawyer" -> Just Lawyer
  "Unemployed" -> Just Unemployed
  _ -> Nothing

newtype Person = Person
  { name :: FullName
  , age :: Age
  , occupation :: Occupation
  }

derive instance Newtype Person _
derive newtype instance Show Person
instance ToCSV Person where
  toCSV person =
    case unwrap person of
      { name, age, occupation } ->
        CSV $
          unwrap name
            <> ","
            <> show (unwrap age)
            <> ","
            <> show occupation

class FromCSV a where
  fromCSV :: CSV -> Maybe a

instance FromCSV Person where
  fromCSV (CSV s) =
    case split (Pattern ",") s of
      [ name, age, occupation ] ->
        let
          mAge = fromString age
          mOccupation = occupationFromString occupation
        in
          case Tuple mAge mOccupation of
            Tuple (Just a) (Just o) ->
              Just $ Person
                { name: wrap name
                , age: wrap a
                , occupation: o
                }
            _ -> Nothing
      _ -> Nothing
