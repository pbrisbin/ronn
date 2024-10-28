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
  )
where

import Prelude

import Data.Text (unpack)
import Ronn.AST
import Ronn.Render

ronnFilePath :: Ronn -> FilePath
ronnFilePath ronn =
  unpack ref.name <> "." <> show (manSectionNumber ref.section) <> ".ronn"
 where
  ref = ronn.name
