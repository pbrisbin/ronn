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
  , Section (..)
  , Content (..)
  , Definition (..)
  , Group (..)
  , Line (..)
  , Part (..)

    -- * References
  , ManRef (..)
  , ManSection (..)
  , manSectionNumber
  ) where

import Prelude

import Data.String (IsString (..))
import Data.Text (Text, pack)
import Ronn.Indent
import Ronn.ManRef

data Ronn = Ronn
  { name :: ManRef
  , description :: [Part]
  , sections :: [Section]
  }

data Section = Section
  { name :: Text
  , content :: [Content]
  }

data Content
  = Definitions [Definition]
  | Groups [Group]

instance IsString Content where
  fromString = Groups . pure . fromString

data Definition = Definition
  { name :: Part
  , description :: Line
  -- ^ A line of nested description is required
  , content :: Maybe [Content]
  -- ^ More content can be optionally nested
  }

data Group
  = Title ManRef [Part]
  | Header Text
  | Lines [Line]

instance IsString Group where
  fromString = Lines . pure . fromString

instance Indentable Group where
  indent n = \case
    g@Title {} -> g
    g@Header {} -> g
    Lines ls -> Lines $ map (indent n) ls

newtype Line = Line
  { unwrap :: [Part]
  }

instance IsString Line where
  fromString = Line . pure . fromString

instance Indentable Line where
  indent n = \case
    Line [] -> Line []
    Line (p : ps) -> Line $ (spaces <> p) : ps
   where
    spaces = Raw $ pack $ replicate (fromIntegral n) ' '

data Part
  = -- | 'Concat' joins 'Part's without automaticaly inserting a space
    --
    -- The following expressions are equivalent:
    --
    -- - @'ronnLineToText' $ 'Line' [p1, p2]@
    -- - @'ronnLineToText' $ 'Line' ['Concat' [p1, " ", p2]]@
    -- - @'ronnLineToText' $ 'Line' [p1 <> " " <> p2]@
    --
    -- Using the 'Semigroup' instance should be preferred, in case the AST
    -- changes in the future.
    Concat [Part]
  | Code Part
  | UserInput Part
  | Strong Part
  | Variable Part
  | Ephasis Part
  | Brackets Part
  | Parens Part
  | Ref ManRef
  | Raw Text

instance IsString Part where
  fromString = Raw . pack

instance Semigroup Part where
  Concat as <> Concat bs = Concat $ as <> bs
  Concat as <> b = Concat $ as <> [b]
  a <> Concat bs = Concat $ a : bs
  a <> b = Concat [a, b]

instance Monoid Part where
  mempty = Concat []
