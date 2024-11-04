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

    -- * Parser-based sections
  , HasSections (..)
  , getSections

    -- * Higher-level builders
  , synopsisSection
  , seeAlsoSection
  , oneLineSection
  , definitionsSection
  )
where

import Prelude

import Data.List (intersperse, sort)
import Data.Maybe (catMaybes)
import Data.Text (Text, unpack)
import Ronn.AST
import Ronn.Render

ronnFilePath :: Ronn -> FilePath
ronnFilePath ronn =
  unpack ronn.title.name.name
    <> "."
    <> show (manSectionNumber ronn.title.name.section)
    <> ".ronn"

-- | Parser types can be made an instance of this for use with 'getSections'
class HasSections p where
  -- | Options parsers should produce @[-f|--foo] --bar@ synopsis parts
  getSynopsis :: p a -> Maybe [Part]
  getSynopsis _ = Nothing

  -- | Options parsers should produce a list of option/help definitions
  getOptDefinitions :: p a -> Maybe [Definition]
  getOptDefinitions _ = Nothing

  -- | Environment parsers should produce a list of variable/help definitions
  getEnvDefinitions :: p a -> Maybe [Definition]
  getEnvDefinitions _ = Nothing

getSections :: HasSections p => Text -> p a -> [Section]
getSections name p =
  catMaybes
    [ synopsisSection name <$> getSynopsis p
    , definitionsSection "OPTIONS" <$> getOptDefinitions p
    , definitionsSection "ENVIRONMENT" <$> getEnvDefinitions p
    ]

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
