{-# OPTIONS_GHC -Wno-orphans #-}

-- |
--
-- Module      : Ronn.OptEnvConf
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Ronn.OptEnvConf
  ( optSynopsis
  , optDefinitions
  , envDefinitions
  ) where

import Prelude

import OptEnvConf (Parser)
import Ronn (HasSections (..))
import Ronn.OptEnvConf.Env
import Ronn.OptEnvConf.Opt

instance HasSections Parser where
  getSynopsis = Just . optSynopsis
  getOptDefinitions = Just . optDefinitions
  getEnvDefinitions = Just . envDefinitions
