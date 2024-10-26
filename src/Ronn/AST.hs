-- |
--
-- Module      : Ronn.AST
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Ronn.AST
  ( Ronn (..)
  , RonnSection (..)
  , RonnContent (..)
  , RonnDefinition (..)
  , RonnGroup (..)
  , RonnLine (..)
  , RonnPart (..)

    -- * References
  , ManRef (..)
  , ManSection (..)
  , manSectionNumber
  ) where

import Prelude

import Data.String (IsString (..))
import Data.Text (Text, pack)

data Ronn = Ronn
  { name :: ManRef
  , description :: [RonnPart]
  , sections :: [RonnSection]
  }

data RonnSection = RonnSection
  { name :: Text
  , content :: [RonnContent]
  }

data RonnContent
  = RonnDefinitions [RonnDefinition]
  | RonnGroups [RonnGroup]

instance IsString RonnContent where
  fromString = RonnGroups . pure . fromString

data RonnDefinition = RonnDefinition
  { name :: RonnPart
  , description :: RonnLine
  -- ^ A line of nested description is required
  , content :: Maybe [RonnContent]
  -- ^ More content can be optionally nested
  }

data RonnGroup
  = RonnTitle ManRef [RonnPart]
  | RonnHeader Text
  | RonnLines [RonnLine]

instance IsString RonnGroup where
  fromString = RonnLines . pure . fromString

newtype RonnLine = RonnLine
  { unwrap :: [RonnPart]
  }

instance IsString RonnLine where
  fromString = RonnLine . pure . fromString

data RonnPart
  = -- | 'RonnConcat' joins 'RonnPart's without automaticaly inserting a space
    --
    -- The following expressions are equivalent:
    --
    -- - @'ronnLineToText' $ 'RonnLine' [p1, p2]@
    -- - @'ronnLineToText' $ 'RonnLine' ['RonnConcat' [p1, " ", p2]]@
    -- - @'ronnLineToText' $ 'RonnLine' [p1 <> " " <> p2]@
    --
    -- Using the 'Semigroup' instance should be preferred, in case the AST
    -- changes in the future.
    RonnConcat [RonnPart]
  | RonnCode RonnPart
  | RonnUserInput RonnPart
  | RonnStrong RonnPart
  | RonnVariable RonnPart
  | RonnEphasis RonnPart
  | RonnBrackets RonnPart
  | RonnParens RonnPart
  | RonnRef ManRef
  | RonnRaw Text

instance IsString RonnPart where
  fromString = RonnRaw . pack

instance Semigroup RonnPart where
  RonnConcat as <> RonnConcat bs = RonnConcat $ as <> bs
  RonnConcat as <> b = RonnConcat $ as <> [b]
  a <> RonnConcat bs = RonnConcat $ a : bs
  a <> b = RonnConcat [a, b]

instance Monoid RonnPart where
  mempty = RonnConcat []

data ManRef = ManRef
  { name :: Text
  , section :: ManSection
  }

-- TODO: enum?
newtype ManSection = ManSection Int

manSectionNumber :: ManSection -> Int
manSectionNumber (ManSection n) = n
