-- |
--
-- Module      : Ronn.OptEnvConf.Opt
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Ronn.OptEnvConf.Opt
  ( optSynopsis
  , optDefinitions
  ) where

import Prelude

import Data.Foldable (toList)
import Data.List (intersperse)
import Data.String (IsString (..))
import Data.Text (pack)
import Data.Text qualified as T
import OptEnvConf (Parser)
import OptEnvConf.Args (Dashed (..))
import OptEnvConf.Doc (AnyDocs (..), OptDoc (..), parserOptDocs)
import Ronn.AST

optSynopsis :: Parser a -> [Part]
optSynopsis = go False . parserOptDocs
 where
  go :: Bool -> AnyDocs (Maybe OptDoc) -> [Part]
  go nested = \case
    AnyDocsCommands {} -> [] -- TODO
    AnyDocsAnd ds -> concatMap (go True) ds
    AnyDocsOr [AnyDocsSingle (Just d)] -> [Brackets $ optDocPart d] -- optional d
    AnyDocsOr ds ->
      [ (if nested then Parens else id) $
          Concat $
            intersperse " \\| " $
              concatMap (go True) ds
      ]
    AnyDocsSingle Nothing -> []
    AnyDocsSingle (Just d) -> [optDocPart d]

optDocPart :: OptDoc -> Part
optDocPart doc = bracketize go
 where
  go
    | null (optDocDasheds doc)
    , Just mv <- optDocMetavar doc =
        case optDocDefault doc of
          Nothing -> Variable $ fromString mv
          Just {} -> Brackets $ fromString mv
    | otherwise =
        Concat $
          intersperse "\\|" $
            optDashedParts doc

  bracketize = case optDocDefault doc of
    Nothing -> id
    Just {} -> Brackets

optDefinitions :: Parser a -> [Definition]
optDefinitions = go . parserOptDocs
 where
  go :: AnyDocs (Maybe OptDoc) -> [Definition]
  go = \case
    AnyDocsCommands {} -> [] -- TODO
    AnyDocsAnd ds -> concatMap go ds
    AnyDocsOr ds -> concatMap go ds
    AnyDocsSingle Nothing -> []
    AnyDocsSingle (Just d) -> [optDocDefinition d]

optDocDefinition :: OptDoc -> Definition
optDocDefinition doc =
  Definition
    { name
    , description =
        Line $
          concat
            [ maybe [] (pure . fromString) $ optDocHelp doc
            , maybe [] (\d -> pure $ Parens $ "default " <> fromString d) $ optDocDefault doc
            ]
    , content = Nothing
    }
 where
  name
    | null (optDocDasheds doc)
    , Just mv <- optDocMetavar doc =
        Code $ fromString mv
    | otherwise = Concat $ intersperse ", " $ optDashedParts doc

optDashedParts :: OptDoc -> [Part]
optDashedParts doc =
  map (\c -> Concat [Code $ "-" <> Raw (T.singleton c)]) shorts
    <> map
      (\n -> Concat $ addArgument (optDocMetavar doc) [Code $ "--" <> Raw (pack n)])
      longs
 where
  (shorts, longs) = partitionDasheds $ optDocDasheds doc

addArgument :: Maybe String -> [Part] -> [Part]
addArgument = \case
  Nothing -> id
  Just arg -> (<> ["=", Variable $ fromString arg])

partitionDasheds :: [Dashed] -> ([Char], [String])
partitionDasheds = go ([], [])
 where
  go acc@(shorts, longs) = \case
    [] -> acc
    (DashedShort c : ds) -> go (shorts <> [c], longs) ds
    (DashedLong cs : ds) -> go (shorts, longs <> [toList cs]) ds
