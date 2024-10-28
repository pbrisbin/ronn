module Ronn.ManRefSpec
  ( spec
  ) where

import Prelude

import Data.List (sort)
import Ronn.ManRef
import Test.Hspec

spec :: Spec
spec = do
  describe "ManRef" $ do
    describe "Ord" $ do
      it "sorts section then name" $ do
        let
          unsorted =
            [ ManRef "groff_man" ManSection7
            , ManRef "man" ManSection1
            , ManRef "man2html" ManSection1
            , ManRef "mdoc" ManSection7
            , ManRef "groff" ManSection7
            , ManRef "man" ManSection7
            , ManRef "attributes" ManSection7
            ]
          sorted =
            [ ManRef "man" ManSection1
            , ManRef "man2html" ManSection1
            , ManRef "attributes" ManSection7
            , ManRef "groff" ManSection7
            , ManRef "groff_man" ManSection7
            , ManRef "man" ManSection7
            , ManRef "mdoc" ManSection7
            ]

        sort unsorted `shouldBe` sorted

  describe "ManSection" $ do
    describe "manSectionNumber" $ do
      it "can be converted to numbers 1 though 8" $ do
        map manSectionNumber [minBound .. maxBound] `shouldBe` [1 .. 8]
