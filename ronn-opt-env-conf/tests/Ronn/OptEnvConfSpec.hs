{-# OPTIONS_GHC -Wno-orphans #-}

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

import OptEnvConf
import Ronn.OptEnvConf
import Ronn.Test
import Test.Hspec

instance HasSynopsis Parser where
  getSynopsis = Just . optSynopsis

instance HasOptDefinitions Parser where
  getOptDefinitions = Just . optDefinitions

instance HasEnvDefinitions Parser where
  getEnvDefinitions = Just . envDefinitions

spec :: Spec
spec = do
  specify "complete example" $
    ronnGolden "opt-env-conf" $
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
          , reader (str @String)
          , metavar "FILE"
          , option
          , env "OUTPUT"
          , value "-"
          ]
        <*> setting
          [ help "Input file"
          , reader (str @String)
          , metavar "FILE"
          , argument
          , env "INPUT"
          ]
