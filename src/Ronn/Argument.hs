module Ronn.Argument
  ( HasArgument (..)
  , addArgument
  )
where

import Prelude

import Data.String (IsString (..))
import Ronn.AST

class HasArgument a where
  getArgument :: a -> Maybe String

addArgument :: HasArgument a => a -> RonnPart -> RonnPart
addArgument a p = case getArgument a of
  Nothing -> p
  Just arg -> mconcat [p, "=", RonnVariable $ fromString arg]
