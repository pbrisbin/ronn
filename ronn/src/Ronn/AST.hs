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
  , Part (..)

    -- * References
  , ManRef (..)
  , ManSection (..)
  , manSectionNumber
  ) where

import Prelude

import Data.String (IsString (..))
import Data.Text (Text, pack)
import Data.Text qualified as T
import Prettyprinter
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

instance Pretty Section where
  pretty s = vsep $ ("##" <+> pretty s.name) : map pretty s.content

data Content
  = -- | Reflowed line
    Para [Part]
  | -- | Unbroken line
    Line [Part]
  | -- | Single definition
    Defn Definition

instance IsString Content where
  fromString = Para . pure . fromString

instance Pretty Content where
  pretty =
    (hardline <>) . \case
      Para ps -> reflow ps
      Line ps -> hsep $ map pretty ps
      Defn dn -> pretty dn

data Definition = Definition
  { name :: Part
  , description :: [Part]
  -- ^ A line of nested description is required
  , content :: Maybe [Content]
  -- ^ More content can be optionally nested
  }

instance Pretty Definition where
  pretty d =
    indent 2
      $ "*"
        <+> align
          ( vsep
              [ pretty d.name <> ":"
              , reflow d.description
              ]
          )
        <> maybe mempty (nest 2 . foldMap ((hardline <>) . pretty)) d.content

data Part
  = -- | 'Concat' joins 'Part's without automaticaly inserting a space
    --
    -- - @'pretty' [p1, p2]@ (may be broken for reflow)
    -- - @'pretty' ['Concat' [p1, " ", p2]]@ (never broken)
    --
    -- '(<>)' is implemented with 'Concat' and should be preferred, to avoid
    -- unnecessary nesting.
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

instance Pretty Part where
  pretty = \case
    Concat ps -> foldMap pretty ps
    Code p -> "`" <> pretty p <> "`"
    UserInput p -> "`" <> pretty p <> "`"
    Strong p -> "**" <> pretty p <> "**"
    Variable p -> "<" <> pretty p <> ">"
    Ephasis p -> "_" <> pretty p <> "_"
    Brackets p -> "[" <> pretty p <> "]"
    Parens p -> "(" <> pretty p <> ")"
    Ref ref -> "**" <> pretty ref <> "**"
    Raw t -> pretty t

-- | Reflow a paragraph by tokenizing words and inserting softlines
--
-- This function will split any 'Raw' parts into multiple 'Raw' parts, one per
-- word, so that 'fillSep' will insert softlines between them.
reflow :: [Part] -> Doc ann
reflow = fillSep . map pretty . concatMap reword
 where
  reword = \case
    Raw t -> map Raw $ T.words t
    p -> [p]
