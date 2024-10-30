-- |
--
-- Module      : Ronn.EnvSpec
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Ronn.EnvSpec
  ( spec
  ) where

import Prelude

import Data.Text (Text, unpack)
import Data.Text.IO qualified as T
import Env
import Ronn
import Ronn.Env
import System.FilePath ((</>))
import Test.Hspec
import Test.Hspec.Golden

spec :: Spec
spec = do
  specify "complete example" $
    let
      p :: Parser Error (Maybe Bool, FilePath, FilePath)
      p =
        (,,)
          <$> optional (switch "DEBUG" $ help "Enable debug")
          <*> var nonempty "OUTPUT" (help "Output file" <> def "-")
          <*> var nonempty "INPUT" (help "Input file")
    in
      ronnGolden $
        Ronn
          { name = ManRef "ronn-envparse" ManSection1
          , description = ["example Ronn from envparse"]
          , sections =
              [ Section
                  { name = "ENVIRONMENT"
                  , content = [Definitions $ envDefinitions p]
                  }
              ]
          }

ronnGolden :: Ronn -> Golden Text
ronnGolden ronn =
  Golden
    { output = ronnToText ronn
    , encodePretty = unpack
    , writeToFile = T.writeFile
    , readFromFile = T.readFile
    , goldenFile = "../doc" </> ronnFilePath ronn
    , actualFile = Nothing
    , failFirstTime = False
    }
