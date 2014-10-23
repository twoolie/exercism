module Allergies (Allergen(..), isAllergicTo, allergies) where

import Data.Word
import Data.Bits

data Allergen = Eggs | Peanuts | Shellfish | Strawberries
              | Tomatoes | Chocolate | Pollen | Cats
     deriving (Show, Enum, Eq)

type Score = Word8

isAllergicTo :: Allergen -> Score -> Bool
isAllergicTo = flip testBit . fromEnum

allergies :: Score -> [Allergen]
allergies s = filter (testBit s . fromEnum) [Eggs .. ]