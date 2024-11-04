module Ronn.Indent
  ( Indentable (..)
  ) where

import Numeric.Natural

class Indentable a where
  indent :: Natural -> a -> a
