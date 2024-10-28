-- |
--
-- Module      : Ronn.OptEnvConf.Env
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Ronn.OptEnvConf.Env
  ( envDefinitions
  ) where

import Prelude

import Data.Foldable (toList)
import Data.List (intersperse)
import Data.String (IsString (..))
import OptEnvConf (Parser)
import OptEnvConf.Doc (AnyDocs (..), EnvDoc (..), parserEnvDocs)
import Ronn.AST

envDefinitions :: Parser a -> [Definition]
envDefinitions = go . parserEnvDocs
 where
  go :: AnyDocs EnvDoc -> [Definition]
  go = \case
    AnyDocsCommands {} -> [] -- TODO
    AnyDocsAnd ds -> concatMap go ds
    AnyDocsOr ds -> concatMap go ds
    AnyDocsSingle d -> [envDocDefinition d]

envDocDefinition :: EnvDoc -> Definition
envDocDefinition doc =
  Definition
    { name
    , description = Line $ maybe [] (pure . fromString) $ envDocHelp doc
    , content = Nothing
    }
 where
  name =
    Concat $
      addArgument $
        intersperse "|" $
          map (Code . fromString) $
            toList $
              envDocVars doc

  addArgument = case envDocMetavar doc of
    Nothing -> id
    Just arg -> (<> ["=", Variable $ fromString arg])
