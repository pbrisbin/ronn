{-# OPTIONS_GHC -Wno-orphans #-}

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

import Options.Applicative
import Ronn.Options.Applicative
import Ronn.Test
import Test.Hspec

instance HasSynopsis Parser where
  getSynopsis = Just . optSynopsis

instance HasOptDefinitions Parser where
  getOptDefinitions = Just . optDefinitions

instance HasEnvDefinitions Parser

spec :: Spec
spec = do
  specify "complete example" $
    ronnGolden "optparse-applicative" $
      (,,)
        <$> optional (switch (long "debug" <> help "Enable debug"))
        <*> option
          (str @String)
          ( mconcat
              [ short 'o'
              , long "output"
              , help "Output file"
              , metavar "FILE"
              , value "-"
              , showDefault
              ]
          )
        <*> argument (str @String) (help "Input file" <> metavar "INPUT")
