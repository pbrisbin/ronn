{-# OPTIONS_GHC -Wno-orphans #-}

-- |
--
-- Module      : Ronn.Env
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Ronn.Env
  ( envDefinitions
  ) where

import Prelude

import Data.String (IsString (..))
import Env (Parser)
import Env.Internal.Help (helpDoc)
import Ronn (HasSections (..))
import Ronn.AST

instance HasSections (Parser e) where
  getEnvDefinitions = Just . envDefinitions

envDefinitions :: Parser e a -> [Definition]
envDefinitions = map fromHelpLine . drop 2 . lines . helpDoc

fromHelpLine :: String -> Definition
fromHelpLine x =
  Definition
    { name
    , description = Line [help]
    , content = Nothing
    }
 where
  (name, help) = case words x of
    (y : ys) -> (fromString y, fromString $ unwords ys)
    _ -> ("", "(error: envparse helpline was empty)")
