-- |
--
-- Module      : Ronn
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Ronn
  ( module Ronn.AST
  , module Ronn.Render
  , ronnFilePath

    -- * Higher-level builders
  , synopsisSection
  , seeAlsoSection
  , oneLineSection
  , definitionsSection
  )
where

import Prelude

import Data.List (intersperse, sort)
import Data.Text (Text, unpack)
import Ronn.AST
import Ronn.Render

ronnFilePath :: Ronn -> FilePath
ronnFilePath ronn =
  unpack ref.name <> "." <> show (manSectionNumber ref.section) <> ".ronn"
 where
  ref = ronn.name

synopsisSection :: Text -> [Part] -> Section
synopsisSection name = oneLineSection "SYNOPSIS" . (Code (Raw name) :)

seeAlsoSection :: [ManRef] -> Section
seeAlsoSection refs =
  oneLineSection
    "SEE ALSO"
    [ mconcat $
        intersperse ", " $
          map Ref $
            sort refs
    ]

oneLineSection :: Text -> [Part] -> Section
oneLineSection name ps =
  Section
    { name
    , content = [Groups [Lines [Line ps]]]
    }

definitionsSection :: Text -> [Definition] -> Section
definitionsSection name definitions =
  Section
    { name
    , content = [Definitions definitions]
    }
