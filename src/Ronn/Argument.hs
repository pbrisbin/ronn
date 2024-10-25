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

addArgument :: HasArgument a => Text -> a -> RonnPart -> RonnPart
addArgument sep a p = case getArgument a of
  Nothing -> p
  Just arg -> mconcat [p, RonnRaw sep, RonnVariable $ fromString arg]
