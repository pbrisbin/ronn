-- |
--
-- Module      : Ronn.OptEnvConfSpec
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Ronn.OptEnvConfSpec
  ( spec
  ) where

import Prelude

import Data.Text (Text, unpack)
import Data.Text.IO qualified as T
import OptEnvConf
import Ronn
import Ronn.OptEnvConf
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
          <$> optional
            ( setting
                [ help "Enable debug"
                , long "debug"
                , switch True
                , env "DEBUG"
                , env "VERBOSE"
                ]
            )
          <*> setting
            [ help "Output file"
            , short 'o'
            , long "output"
            , reader str
            , metavar "FILE"
            , option
            , env "OUTPUT"
            , value "-"
            ]
          <*> setting
            [ help "Input file"
            , reader str
            , metavar "FILE"
            , argument
            , env "INPUT"
            ]
    in
      ronnGolden $
        Ronn
          { name = ManRef "ronn-opt-env-conf" ManSection1
          , description = ["example Ronn from opt-env-conf"]
          , sections =
              [ Section
                  { name = "SYNOPSIS"
                  , content = [Groups [Lines [Line $ Code "ronn-opt-env-conf" : optSynopsis p]]]
                  }
              , Section
                  { name = "OPTIONS"
                  , content = [Definitions $ optDefinitions p]
                  }
              , Section
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
