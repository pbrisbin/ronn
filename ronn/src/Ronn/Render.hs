-- |
--
-- Module      : Ronn.Render
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Ronn.Render
  ( ronnToText
  , ronnToDoc
  ) where

import Prelude

import Data.Text (Text)
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import Ronn.AST

ronnToText :: Ronn -> Text
ronnToText = renderStrict . layoutPretty defaultLayoutOptions . ronnToDoc

ronnToDoc :: Ronn -> Doc ann
ronnToDoc ronn =
  (<> hardline) -- ensure doc ends in final newline
    $ vsep
    $ punctuate hardline
    $ prettyTitle ronn
      : map pretty ronn.sections

prettyTitle :: Ronn -> Doc ann
prettyTitle ronn =
  underline '='
    $ pretty ronn.name <+> "--" <+> hsep (map pretty ronn.description)

underline :: Char -> Doc ann -> Doc ann
underline c x = width x $ \w -> hardline <> pretty (replicate w c)
