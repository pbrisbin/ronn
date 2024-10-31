{-# OPTIONS_GHC -Wno-orphans #-}

-- |
--
-- Module      : Ronn.Options.Applicative
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Ronn.Options.Applicative
  ( optSynopsis
  , optDefinitions
  ) where

import Prelude

import Control.Monad (void)
import Data.List (intersperse, sortBy)
import Data.String (IsString (..))
import Data.Text (Text)
import Options.Applicative (Parser)
import Options.Applicative.Common (mapParser, treeMapParser)
import Options.Applicative.Help.Chunk (Chunk (..))
import Options.Applicative.Help.Pretty (Doc)
import Options.Applicative.Help.Pretty qualified as Pretty
import Options.Applicative.Types
  ( OptName (..)
  , OptProperties (..)
  , OptReader (..)
  , OptTree (..)
  , Option (..)
  )
import Ronn (HasSections (..))
import Ronn.AST

instance HasSections Parser where
  getSynopsis = Just . optSynopsis
  getOptDefinitions = Just . optDefinitions

optSynopsis :: Parser a -> [Part]
optSynopsis = go False . treeMapParser (const void)
 where
  go :: Bool -> OptTree (Option ()) -> [Part]
  go nested = \case
    Leaf o -> [optSynopsisPart o]
    MultNode ts -> concatMap (go True) ts
    AltNode _ [Leaf o]
      | FlagReader {} <- optMain o -> [Brackets $ optSynopsisPart o] -- optional o
      | otherwise -> [optSynopsisPart o]
    AltNode _ ts ->
      [ (if nested then Parens else id) $
          Concat $
            intersperse " \\| " $
              concatMap (go True) ts
      ]
    BindNode t -> go True t

optSynopsisPart :: Option x -> Part
optSynopsisPart o = bracketize go
 where
  go = case optMain o of
    OptReader names _ _ ->
      let mv = propMetaVar $ optProps o
      in  Concat $ intersperse "\\|" $ renderNames (Just mv) names
    FlagReader names _ ->
      Concat $ intersperse "\\|" $ renderNames Nothing names
    ArgReader {} ->
      case propShowDefault $ optProps o of
        Nothing -> Variable $ fromString $ propMetaVar $ optProps o
        Just {} -> Brackets $ fromString $ propMetaVar $ optProps o
    CmdReader {} -> "" -- TODO
  bracketize = case propShowDefault $ optProps o of
    Nothing -> id
    Just {} -> Brackets

optDefinitions :: Parser a -> [Definition]
optDefinitions = mapParser optDefinition

optDefinition :: a -> Option x -> Definition
optDefinition _ o =
  Definition
    { name = case optMain o of
        OptReader names _ _ ->
          let mv = propMetaVar $ optProps o
          in  Concat $ intersperse ", " $ renderNames (Just mv) names
        FlagReader names _ ->
          Concat $ intersperse ", " $ renderNames Nothing names
        ArgReader {} -> Code $ fromString $ propMetaVar $ optProps o
        CmdReader {} -> undefined -- TODO
    , description =
        let
          help = Raw (docToText $ propHelp $ optProps o)
          suffix =
            maybe [] (pure . Parens . ("default " <>) . fromString) $
              propShowDefault $
                optProps o
        in
          Line $ help : suffix
    , content = Nothing
    }

docToText :: Chunk Doc -> Text
docToText =
  maybe
    ""
    ( Pretty.renderStrict
        . Pretty.layoutPretty Pretty.defaultLayoutOptions
    )
    . unChunk

renderNames :: Maybe String -> [OptName] -> [Part]
renderNames mArg = map (renderName mArg) . sortNames

renderName :: Maybe String -> OptName -> Part
renderName mArg = \case
  OptShort c -> Code $ fromString ['-', c]
  OptLong st -> addArg $ Code $ fromString $ "--" <> st
 where
  addArg :: Part -> Part
  addArg = case mArg of
    Nothing -> id
    Just arg -> Concat . (<> ["=", Variable $ fromString arg]) . pure

sortNames :: [OptName] -> [OptName]
sortNames = sortBy $ curry $ \case
  (OptShort a, OptShort b) -> compare a b
  (OptShort _, OptLong _) -> LT
  (OptLong a, OptLong b) -> compare a b
  (OptLong _, OptShort _) -> GT
