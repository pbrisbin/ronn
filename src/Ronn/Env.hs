-- |
--
-- Module      : Ronn.Env
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Ronn.Env
  ( Env (..)
  , envToDefinition
  , environmentSection
  ) where

import Prelude

import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Ronn.AST
import Ronn.Argument

data Env = Env
  { vars :: [String]
  , argument :: Maybe String
  , default_ :: Maybe String
  , help :: Maybe RonnLine
  }

instance HasArgument Env where
  getArgument = (.argument)

envToDefinition :: Env -> RonnDefinition
envToDefinition env =
  RonnDefinition
    { name =
        addArgument "=" env $
          RonnConcat $
            intersperse "|" $
              map (RonnCode . fromString) env.vars
    , description = fromMaybe (RonnLine []) env.help
    , content = Nothing
    }

environmentSection :: [Env] -> RonnSection
environmentSection envs =
  RonnSection
    { name = "ENVIRONMENT"
    , content = [RonnDefinitions $ map envToDefinition envs]
    }
