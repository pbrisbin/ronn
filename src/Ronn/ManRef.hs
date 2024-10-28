module Ronn.ManRef
  ( ManRef (..)
  , ManSection (..)
  , manSectionNumber
  )
where

import Prelude

import Data.Ord
import Data.Text

data ManRef = ManRef
  { name :: Text
  , section :: ManSection
  }
  deriving stock (Eq, Show)

instance Ord ManRef where
  compare a b = comparing (.section) a b <> comparing (.name) a b

data ManSection
  = ManSection1
  | ManSection2
  | ManSection3
  | ManSection4
  | ManSection5
  | ManSection6
  | ManSection7
  | ManSection8
  deriving stock (Eq, Ord, Bounded, Enum, Show)

manSectionNumber :: ManSection -> Int
manSectionNumber = (+ 1) . fromEnum
