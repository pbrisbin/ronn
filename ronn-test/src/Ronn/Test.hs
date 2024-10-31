-- |
--
-- Module      : Ronn.Test
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Ronn.Test
  ( ronnGolden
  ) where

import Prelude

import Data.Text (Text, unpack)
import Data.Text.IO qualified as T
import Ronn
import System.FilePath ((</>))
import Test.Hspec.Golden

ronnGolden :: HasSections p => Text -> p a -> Golden Text
ronnGolden name p =
  Golden
    { output = ronnToText ronn
    , encodePretty = unpack
    , writeToFile = T.writeFile
    , readFromFile = T.readFile
    , goldenFile = "../doc" </> ronnFilePath ronn
    , actualFile = Nothing
    , failFirstTime = False
    }
 where
  ronn =
    Ronn
      { name = ManRef ("ronn-" <> name) ManSection1
      , description = ["example Ronn from", Raw name]
      , sections = getSections ("ronn-" <> name) p
      }
