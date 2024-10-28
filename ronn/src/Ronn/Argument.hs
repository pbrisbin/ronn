-- |
--
-- Module      : Ronn.Argument
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Ronn.Argument
  ( HasArgument (..)
  , addArgument
  )
where

import Prelude

import Data.String (IsString (..))
import Data.Text (Text)
import Ronn.AST

class HasArgument a where
  getArgument :: a -> Maybe String

addArgument :: HasArgument a => Text -> a -> Part -> Part
addArgument sep a p = case getArgument a of
  Nothing -> p
  Just arg -> mconcat [p, Raw sep, Variable $ fromString arg]
