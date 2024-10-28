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
  ) where

import Prelude

import Data.Text (Text, pack)
import Data.Text qualified as T
import Ronn.AST

ronnToText :: Ronn -> Text
ronnToText ronn =
  T.unlines $
    map ronnGroupToText $
      Title ronn.name ronn.description
        : concatMap ronnSectionToGroups ronn.sections

ronnSectionToGroups :: Section -> [Group]
ronnSectionToGroups section =
  Header section.name : concatMap (ronnContentToGroups 0) section.content

ronnContentToGroups :: Int -> Content -> [Group]
ronnContentToGroups indentLevel = \case
  Definitions defns ->
    concatMap (ronnDefinitionToGroups $ indentLevel + 2) defns
  Groups gs -> map (indentRonnGroup indentLevel) gs

ronnDefinitionToGroups :: Int -> Definition -> [Group]
ronnDefinitionToGroups indentLevel defn =
  [ Lines
      [ indentRonnLine indentLevel $ Line ["* " <> defn.name <> ":"]
      , indentRonnLine nextIndentLevel defn.description
      ]
  ]
    <> nested
 where
  nested = maybe [] (concatMap $ ronnContentToGroups nextIndentLevel) defn.content
  nextIndentLevel = indentLevel + 2

ronnGroupToText :: Group -> Text
ronnGroupToText = T.unlines . map ronnLineToText . ronnGroupToLines

ronnGroupToLines :: Group -> [Line]
ronnGroupToLines = \case
  Title ref description ->
    let nameLine = ronnNameLine ref description
    in  [ nameLine
        , Line [Raw $ T.replicate (ronnLineLength nameLine) "="]
        ]
  Header name -> [Line ["##", Raw name]]
  Lines ls -> ls

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

indentRonnGroup :: Int -> Group -> Group
indentRonnGroup indentLevel = \case
  g@Title {} -> g
  g@Header {} -> g
  Lines ls -> Lines $ map (indentRonnLine indentLevel) ls

-- | Prepends the given number of spaces to the first 'Part' of the line
indentRonnLine :: Int -> Line -> Line
indentRonnLine n = \case
  Line [] -> Line []
  Line (p : ps) -> Line $ (spaces <> p) : ps
 where
  spaces = Raw $ pack $ replicate n ' '
