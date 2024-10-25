module Ronn.Opt
  ( Opt (..)
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
    { name = optPartComma opt
    , description = fromMaybe (RonnLine []) opt.help
    , content = Nothing
    }

-- | Render an 'Opt' to a pipe-separated element
--
-- Example: @`-f`|`--foo`=<ARG>@
optPartPipe :: Opt -> RonnPart
optPartPipe = mconcat . intersperse "\\|" . optParts

-- | Render an 'Opt' to a comma-separated element
--
-- Example: @`-f`, `--foo`=<ARG>@
optPartComma :: Opt -> RonnPart
optPartComma = mconcat . intersperse ", " . optParts

optParts :: Opt -> [RonnPart]
optParts opt
  | null opt.shorts && null opt.longs
  , Just arg <- opt.argument =
      [RonnVariable $ fromString arg]
  | otherwise =
      map short opt.shorts <> map (addArgument opt . long) opt.longs
 where
  short :: Char -> RonnPart
  short = RonnCode . fromString . ('-' :) . pure

  long :: String -> RonnPart
  long = RonnCode . fromString . ("--" <>)

optionsSection :: [Opt] -> RonnSection
optionsSection opts =
  RonnSection
    { name = "OPTIONS"
    , content = [RonnDefinitions $ map optToDefinition opts]
    }

synopsisSection
  :: String
  -- ^ Program name
  -> [Opt]
  -> RonnSection
synopsisSection name opts =
  RonnSection
    { name = "SYNOPSIS"
    , content = [RonnGroups [RonnLines [synopsisLine name opts]]]
    }

synopsisLine :: String -> [Opt] -> RonnLine
synopsisLine name =
  RonnLine
    . (RonnCode (fromString name) :)
    . map (\opt -> bracketize opt $ optPartPipe opt)

bracketize :: Opt -> RonnPart -> RonnPart
bracketize opt
  | isJust opt.default_ = RonnBrackets
  | otherwise = id
