{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Ronn.RenderSpec
  ( spec
  ) where

import Prelude

import Data.List (intersperse)
import Data.Text qualified as T
import Ronn.AST
import Ronn.Render
import Test.Hspec

spec :: Spec
spec = do
  describe "ronnToText" $ do
    specify "a complete example" $ do
      let ronn =
            Ronn
              { name = ManRef "ronn" $ ManSection 1
              , description = ["example ronn man-page"]
              , sections =
                  [ RonnSection
                      { name = "SYNOPSIS"
                      , content =
                          [ RonnGroups
                              [ RonnLines
                                  [ RonnLine
                                      [ RonnCode "ronn"
                                      , RonnBrackets $ RonnCode "-h"
                                      , RonnBrackets $ RonnCode "--help"
                                      , RonnBrackets $ mconcat [RonnCode "--debug", "|", RonnCode "--trace"]
                                      , RonnBrackets $ mconcat [RonnCode "-o", " ", RonnVariable "FILE"]
                                      , RonnBrackets $ mconcat [RonnCode "--output", "=", RonnVariable "FILE"]
                                      , RonnVariable "INPUT"
                                      ]
                                  ]
                              ]
                          ]
                      }
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
                  , RonnSection
                      { name = "OPTIONS"
                      , content =
                          [ RonnDefinitions
                              [ RonnDefinition
                                  { name =
                                      mconcat
                                        [ RonnCode "-h"
                                        , ", "
                                        , RonnCode "--help"
                                        ]
                                  , description = "Display this help"
                                  , content = Nothing
                                  }
                              , RonnDefinition
                                  { name = RonnCode "--debug"
                                  , description = "Enable debug"
                                  , content = Nothing
                                  }
                              , RonnDefinition
                                  { name = RonnCode "--trace"
                                  , description = "Enable trace"
                                  , content = Nothing
                                  }
                              , RonnDefinition
                                  { name =
                                      mconcat
                                        [ RonnCode "-o"
                                        , ", "
                                        , RonnCode "--output"
                                        , "="
                                        , RonnVariable "FILE"
                                        ]
                                  , description = RonnLine ["Output to", RonnVariable "FILE"]
                                  , content = Nothing
                                  }
                              , RonnDefinition
                                  { name = RonnVariable "INPUT"
                                  , description = "Source input"
                                  , content = Nothing
                                  }
                              ]
                          ]
                      }
                  , RonnSection
                      { name = "ENVIRONMENT"
                      , content =
                          [ RonnDefinitions
                              [ RonnDefinition
                                  { name = RonnCode "HOME"
                                  , description = "User's HOME directory"
                                  , content =
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
                                      [ mconcat $
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
          , "`ronn` [`-h`] [`--help`] [`--debug`|`--trace`] [`-o` <FILE>] [`--output`=<FILE>] <INPUT>"
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
          , "  * `--debug`:"
          , "    Enable debug"
          , ""
          , "  * `--trace`:"
          , "    Enable trace"
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
