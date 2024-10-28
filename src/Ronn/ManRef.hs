module Ronn.ManRef
  ( ManRef (..)
  , ManSection (..)
  , manSectionNumber
  )
where

import Prelude

import Data.Text

data ManRef = ManRef
  { name :: Text
  , section :: ManSection
  }

-- TODO: enum?
newtype ManSection = ManSection Int

manSectionNumber :: ManSection -> Int
manSectionNumber (ManSection n) = n
