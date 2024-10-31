-- |
--
-- Module      : Ronn.Test
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Ronn.Test
  ( HasSynopsis (..)
  , HasOptDefinitions (..)
  , HasEnvDefinitions (..)
  , ronnGolden
  ) where

import Prelude

import Data.Maybe (catMaybes)
import Data.Text (Text, unpack)
import Data.Text.IO qualified as T
import Ronn
import System.FilePath ((</>))
import Test.Hspec.Golden

class HasSynopsis p where
  getSynopsis :: p a -> Maybe [Part]
  getSynopsis _ = Nothing

class HasOptDefinitions p where
  getOptDefinitions :: p a -> Maybe [Definition]
  getOptDefinitions _ = Nothing

class HasEnvDefinitions p where
  getEnvDefinitions :: p a -> Maybe [Definition]
  getEnvDefinitions _ = Nothing

ronnGolden
  :: ( HasSynopsis p
     , HasOptDefinitions p
     , HasEnvDefinitions p
     )
  => Text
  -> p a
  -> Golden Text
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
      , sections =
          catMaybes
            [ ( \s ->
                  Section
                    { name = "SYNOPSIS"
                    , content = [Groups [Lines [Line $ Code (Raw $ "ronn-" <> name) : s]]]
                    }
              )
                <$> getSynopsis p
            , ( \ds ->
                  Section
                    { name = "OPTIONS"
                    , content = [Definitions ds]
                    }
              )
                <$> getOptDefinitions p
            , ( \ds ->
                  Section
                    { name = "ENVIRONMENT"
                    , content = [Definitions ds]
                    }
              )
                <$> getEnvDefinitions p
            ]
      }
