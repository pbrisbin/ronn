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
      RonnTitle ronn.name ronn.description
        : concatMap ronnSectionToGroups ronn.sections

ronnSectionToGroups :: RonnSection -> [RonnGroup]
ronnSectionToGroups section =
  RonnHeader section.name : concatMap (ronnContentToGroups 0) section.content

ronnContentToGroups :: Int -> RonnContent -> [RonnGroup]
ronnContentToGroups indentLevel = \case
  RonnDefinitions defns ->
    concatMap (ronnDefinitionToGroups $ indentLevel + 2) defns
  RonnGroups gs -> map (indentRonnGroup indentLevel) gs

ronnDefinitionToGroups :: Int -> RonnDefinition -> [RonnGroup]
ronnDefinitionToGroups indentLevel defn =
  [ RonnLines
      [ indentRonnLine indentLevel $ RonnLine ["* " <> defn.name <> ":"]
      , indentRonnLine nextIndentLevel defn.description
      ]
  ]
    <> nested
 where
  nested = maybe [] (concatMap $ ronnContentToGroups nextIndentLevel) defn.content
  nextIndentLevel = indentLevel + 2

ronnGroupToText :: RonnGroup -> Text
ronnGroupToText = T.unlines . map ronnLineToText . ronnGroupToLines

ronnGroupToLines :: RonnGroup -> [RonnLine]
ronnGroupToLines = \case
  RonnTitle ref description ->
    let nameLine = ronnNameLine ref description
    in  [ nameLine
        , RonnLine [RonnRaw $ T.replicate (ronnLineLength nameLine) "="]
        ]
  RonnHeader name -> [RonnLine ["##", RonnRaw name]]
  RonnLines ls -> ls

ronnNameLine :: ManRef -> [RonnPart] -> RonnLine
ronnNameLine ref =
  RonnLine . (RonnRaw (manRefToText ref) :) . ("--" :)

ronnLineToText :: RonnLine -> Text
ronnLineToText = T.unwords . map ronnPartToText . (.unwrap)

ronnPartToText :: RonnPart -> Text
ronnPartToText = \case
  RonnConcat xs -> mconcat $ map ronnPartToText xs
  RonnCode x -> "`" <> ronnPartToText x <> "`"
  RonnUserInput x -> "`" <> ronnPartToText x <> "`"
  RonnStrong x -> "**" <> ronnPartToText x <> "**"
  RonnVariable x -> "<" <> ronnPartToText x <> ">"
  RonnEphasis x -> "_" <> ronnPartToText x <> "_"
  RonnBrackets x -> "[" <> ronnPartToText x <> "]"
  RonnParens x -> "(" <> ronnPartToText x <> ")"
  RonnRef ref -> ronnPartToText $ RonnStrong $ RonnRaw $ manRefToText ref
  RonnRaw x -> x

manRefToText :: ManRef -> Text
manRefToText ref =
  ref.name
    <> "("
    <> pack (show $ manSectionNumber ref.section)
    <> ")"

ronnLineLength :: RonnLine -> Int
ronnLineLength = T.length . ronnLineToText

indentRonnGroup :: Int -> RonnGroup -> RonnGroup
indentRonnGroup indentLevel = \case
  g@RonnTitle {} -> g
  g@RonnHeader {} -> g
  RonnLines ls -> RonnLines $ map (indentRonnLine indentLevel) ls

-- | Prepends the given number of spaces to the first 'RonnPart' of the line
indentRonnLine :: Int -> RonnLine -> RonnLine
indentRonnLine n = \case
  RonnLine [] -> RonnLine []
  RonnLine (p : ps) -> RonnLine $ (spaces <> p) : ps
 where
  spaces = RonnRaw $ pack $ replicate n ' '
