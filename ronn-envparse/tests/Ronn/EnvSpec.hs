{-# OPTIONS_GHC -Wno-orphans #-}

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

import Env
import Ronn.Env
import Ronn.Test
import Test.Hspec

instance HasSynopsis (Parser e)

instance HasOptDefinitions (Parser e)

instance HasEnvDefinitions (Parser e) where
  getEnvDefinitions = Just . envDefinitions

spec :: Spec
spec = do
  specify "complete example" $
    ronnGolden "envparse" $
      (,,)
        <$> optional (switch "DEBUG" $ help "Enable debug")
        <*> var (nonempty @Error @String) "OUTPUT" (help "Output file" <> def "-")
        <*> var (nonempty @Error @String) "INPUT" (help "Input file")
