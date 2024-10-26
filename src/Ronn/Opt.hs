-- |
--
-- Module      : Ronn.Opt
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Ronn.Opt
  ( Opts (..)
  , optsToDefinitions
  , Opt (..)
  , optToDefinition
  , optionsSection
  , synopsisSection
  , synopsisLine
  ) where

import Prelude

import Data.List (intersperse)
import Data.Maybe (fromMaybe, isJust)
import Data.String (IsString (..))
import Ronn.AST
import Ronn.Argument

data Opts
  = OptsOr Opts
  | OptsAnd Opts
  | OptsMany [Opts]
  | OptsOne Opt

optsToDefinitions :: Opts -> [RonnDefinition]
optsToDefinitions = \case
  OptsOr opts -> optsToDefinitions opts
  OptsAnd opts -> optsToDefinitions opts
  OptsMany opts -> concatMap optsToDefinitions opts
  OptsOne opts -> [optToDefinition opts]

data Opt = Opt
  { shorts :: [Char]
  , longs :: [String]
  , argument :: Maybe String
  , default_ :: Maybe String
  , help :: Maybe RonnLine
  }

instance HasArgument Opt where
  getArgument = (.argument)

optToDefinition :: Opt -> RonnDefinition
optToDefinition opt =
  RonnDefinition
    { name = mconcat $ intersperse ", " $ optParts opt
    , description = fromMaybe (RonnLine []) opt.help
    , content = Nothing
    }

optionsSection :: Opts -> RonnSection
optionsSection opts =
  RonnSection
    { name = "OPTIONS"
    , content = [RonnDefinitions $ optsToDefinitions opts]
    }

synopsisSection
  :: String
  -- ^ Program name
  -> Opts
  -> RonnSection
synopsisSection name opts =
  RonnSection
    { name = "SYNOPSIS"
    , content = [RonnGroups [RonnLines [synopsisLine name opts]]]
    }

synopsisLine :: String -> Opts -> RonnLine
synopsisLine name = RonnLine . (RonnCode (fromString name) :) . go
 where
  go :: Opts -> [RonnPart]
  go = \case
    OptsOr os -> [RonnBrackets $ mconcat $ intersperse " \\| " $ go os]
    OptsAnd os -> [RonnParens $ mconcat $ intersperse " " $ go os]
    OptsMany os -> concatMap go os
    OptsOne opt -> map (bracketize opt) $ optParts opt

  bracketize :: Opt -> RonnPart -> RonnPart
  bracketize opt
    | isJust opt.default_ = RonnBrackets
    | otherwise = id

optParts :: Opt -> [RonnPart]
optParts opt
  | null opt.shorts && null opt.longs
  , Just arg <- opt.argument =
      [RonnVariable $ fromString arg]
  | otherwise =
      map (addArgument " " opt . short) opt.shorts
        <> map (addArgument "=" opt . long) opt.longs
 where
  short :: Char -> RonnPart
  short = RonnCode . fromString . ('-' :) . pure

  long :: String -> RonnPart
  long = RonnCode . fromString . ("--" <>)
