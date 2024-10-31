-- |
--
-- Module      : Ronn.Options.ApplicativeSpec
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Ronn.Options.ApplicativeSpec
  ( spec
  ) where

import Prelude

import Data.Text (Text, unpack)
import Data.Text.IO qualified as T
import Options.Applicative
import Ronn
import Ronn.Options.Applicative
import System.FilePath ((</>))
import Test.Hspec
import Test.Hspec.Golden

spec :: Spec
spec = do
  specify "complete example" $
    let
      p :: Parser (Maybe Bool, FilePath, FilePath)
      p =
        (,,)
          <$> optional (switch (long "debug" <> help "Enable debug"))
          <*> option
            str
            ( mconcat
                [ short 'o'
                , long "output"
                , help "Output file"
                , metavar "FILE"
                , value "-"
                , showDefault
                ]
            )
          <*> argument str (help "Input file" <> metavar "INPUT")
    in
      ronnGolden $
        Ronn
          { name = ManRef "ronn-optparse-applicative" ManSection1
          , description = ["example Ronn from optparse-applicative"]
          , sections =
              [ Section
                  { name = "SYNOPSIS"
                  , content =
                      [Groups [Lines [Line $ Code "ronn-optparse-applicative" : optSynopsis p]]]
                  }
              , Section
                  { name = "OPTIONS"
                  , content = [Definitions $ optDefinitions p]
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
