-- |
--
-- Module      : Ronn.Render
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Ronn.Render
  ( ronnToText

    -- * Rendering sub-parts, mostly useful in tests
  , ronnGroupToText
  , ronnLineToText
  , ronnPartToText
  ) where

import Prelude

import Data.Text (Text, pack)
import Data.Text qualified as T
import Numeric.Natural
import Ronn.AST
import Ronn.Indent

ronnToText :: Ronn -> Text
ronnToText ronn =
  T.unlines $
    map ronnGroupToText $
      ronnTitleToGroup ronn.title
        : concatMap ronnSectionToGroups ronn.sections

ronnTitleToGroup :: Title -> Group
ronnTitleToGroup title =
  Lines
    [ nameLine
    , Line [Raw $ T.replicate (ronnLineLength nameLine) "="]
    ]
 where
  nameLine = ronnNameLine title.name title.description

ronnSectionToGroups :: Section -> [Group]
ronnSectionToGroups section =
  Lines [Line ["##", Raw section.name]]
    : concatMap (ronnContentToGroups 0) section.content

ronnContentToGroups :: Natural -> Content -> [Group]
ronnContentToGroups indentLevel = \case
  Definitions defns ->
    concatMap (ronnDefinitionToGroups $ indentLevel + 2) defns
  Groups gs -> map (indent indentLevel) gs

ronnDefinitionToGroups :: Natural -> Definition -> [Group]
ronnDefinitionToGroups indentLevel defn =
  [ Lines
      [ indent indentLevel $ Line ["* " <> defn.name <> ":"]
      , indent nextIndentLevel defn.description
      ]
  ]
    <> nested
 where
  nested = maybe [] (concatMap $ ronnContentToGroups nextIndentLevel) defn.content
  nextIndentLevel = indentLevel + 2

ronnGroupToText :: Group -> Text
ronnGroupToText = T.unlines . map ronnLineToText . (.unwrap)

ronnNameLine :: ManRef -> [Part] -> Line
ronnNameLine ref =
  Line . (Raw (manRefToText ref) :) . ("--" :)

ronnLineToText :: Line -> Text
ronnLineToText = T.unwords . map ronnPartToText . (.unwrap)

ronnPartToText :: Part -> Text
ronnPartToText = \case
  Concat xs -> mconcat $ map ronnPartToText xs
  Code x -> "`" <> ronnPartToText x <> "`"
  UserInput x -> "`" <> ronnPartToText x <> "`"
  Strong x -> "**" <> ronnPartToText x <> "**"
  Variable x -> "<" <> ronnPartToText x <> ">"
  Ephasis x -> "_" <> ronnPartToText x <> "_"
  Brackets x -> "[" <> ronnPartToText x <> "]"
  Parens x -> "(" <> ronnPartToText x <> ")"
  Ref ref -> ronnPartToText $ Strong $ Raw $ manRefToText ref
  Raw x -> x

manRefToText :: ManRef -> Text
manRefToText ref =
  ref.name
    <> "("
    <> pack (show $ manSectionNumber ref.section)
    <> ")"

ronnLineLength :: Line -> Int
ronnLineLength = T.length . ronnLineToText
