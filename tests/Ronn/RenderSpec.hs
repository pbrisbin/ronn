{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Ronn.RenderSpec
  ( spec
  ) where

import Prelude

import Data.List (intersperse)
import Data.Text qualified as T
import Ronn.AST
import Ronn.Env
import Ronn.Opt
import Ronn.Render
import Test.Hspec

spec :: Spec
spec = do
  describe "ronnToText" $ do
    specify "a complete example" $ do
      let
        opts =
          [ Opt
              { shorts = ['h']
              , longs = ["help"]
              , argument = Nothing
              , default_ = Just ""
              , help = Just "Display this help"
              }
          , Opt
              { shorts = ['o']
              , longs = ["output"]
              , argument = Just "FILE"
              , default_ = Just "-"
              , help = Just $ RonnLine ["Output to", RonnVariable "FILE"]
              }
          , Opt
              { shorts = []
              , longs = []
              , argument = Just "INPUT"
              , default_ = Nothing
              , help = Just "Source input"
              }
          ]

        ronn =
          Ronn
            { name = ManRef "ronn" $ ManSection 1
            , description = ["example ronn man-page"]
            , sections =
                [ synopsisSection "ronn" opts
                , RonnSection
                    { name = "DESCRIPTION"
                    , content =
                        [ RonnGroups
                            [ RonnLines
                                [ RonnLine -- test unwords-like behavior
                                    [ "This is an"
                                    , "example man-page to show"
                                    , "how rendering the AST looks."
                                    ]
                                ]
                            ]
                        ]
                    }
                , optionsSection opts
                , RonnSection
                    { name = "ENVIRONMENT"
                    , content =
                        [ RonnDefinitions
                            [ ( envToDefinition $
                                  Env
                                    { vars = ["HOME"]
                                    , argument = Nothing
                                    , default_ = Nothing
                                    , help = Just "User's HOME directory"
                                    }
                              )
                                { content =
                                    Just
                                      [ "Some further details:"
                                      , RonnDefinitions
                                          [ RonnDefinition
                                              { name = "foo"
                                              , description = "The foo"
                                              , content = Nothing
                                              }
                                          , RonnDefinition
                                              { name = "bar"
                                              , description = "The bar"
                                              , content = Nothing
                                              }
                                          ]
                                      ]
                                }
                            ]
                        ]
                    }
                , RonnSection
                    { name = "SEE ALSO"
                    , content =
                        [ RonnGroups
                            [ RonnLines
                                [ RonnLine
                                    [ RonnConcat $
                                        intersperse
                                          (RonnRaw ", ")
                                          [ RonnRef $ ManRef "markdown" $ ManSection 7
                                          , RonnRef $ ManRef "roff" $ ManSection 7
                                          ]
                                    ]
                                ]
                            ]
                        ]
                    }
                ]
            }

      ronnToText ronn
        `shouldBe` T.unlines
          [ "ronn(1) -- example ronn man-page"
          , "================================"
          , ""
          , "## SYNOPSIS"
          , ""
          , "`ronn` [`-h`\\|`--help`] [`-o`\\|`--output`=<FILE>] <INPUT>"
          , ""
          , "## DESCRIPTION"
          , ""
          , "This is an example man-page to show how rendering the AST looks."
          , ""
          , "## OPTIONS"
          , ""
          , "  * `-h`, `--help`:"
          , "    Display this help"
          , ""
          , "  * `-o`, `--output`=<FILE>:"
          , "    Output to <FILE>"
          , ""
          , "  * <INPUT>:"
          , "    Source input"
          , ""
          , "## ENVIRONMENT"
          , ""
          , "  * `HOME`:"
          , "    User's HOME directory"
          , ""
          , "    Some further details:"
          , ""
          , "      * foo:"
          , "        The foo"
          , ""
          , "      * bar:"
          , "        The bar"
          , ""
          , "## SEE ALSO"
          , ""
          , "**markdown(7)**, **roff(7)**"
          , ""
          ]
