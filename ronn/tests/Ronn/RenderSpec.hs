-- |
--
-- Module      : Ronn.RenderSpec
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Ronn.RenderSpec
  ( spec
  ) where

import Prelude

import Data.List (intersperse)
import Data.Text (Text, unpack)
import Data.Text.IO qualified as T
import Ronn
import System.FilePath ((</>))
import Test.Hspec
import Test.Hspec.Golden

spec :: Spec
spec = do
  describe "ronnToText" $ do
    specify "a complete example" $
      ronnGolden $
        Ronn
          { title =
              Title
                { name = ManRef "ronn" ManSection1
                , description = ["example ronn man-page"]
                }
          , sections =
              [ Section
                  { name = "SYNOPSIS"
                  , content =
                      [ Groups
                          [ Lines
                              [ Line
                                  [ Code "ronn"
                                  , Brackets $ Code "-h"
                                  , Brackets $ Code "--help"
                                  , Brackets $ mconcat [Code "--debug", "\\|", Code "--trace"]
                                  , Brackets $ mconcat [Code "-o", " ", Variable "FILE"]
                                  , Brackets $ mconcat [Code "--output", "=", Variable "FILE"]
                                  , Variable "INPUT"
                                  ]
                              ]
                          ]
                      ]
                  }
              , Section
                  { name = "DESCRIPTION"
                  , content =
                      [ Groups
                          [ Lines
                              [ Line -- test unwords-like behavior
                                  [ "This is an"
                                  , "example man-page to show"
                                  , "how rendering the AST looks."
                                  ]
                              ]
                          ]
                      ]
                  }
              , Section
                  { name = "OPTIONS"
                  , content =
                      [ Definitions
                          [ Definition
                              { name =
                                  mconcat
                                    [ Code "-h"
                                    , ", "
                                    , Code "--help"
                                    ]
                              , description = "Display this help"
                              , content = Nothing
                              }
                          , Definition
                              { name = Code "--debug"
                              , description = "Enable debug"
                              , content = Nothing
                              }
                          , Definition
                              { name = Code "--trace"
                              , description = "Enable trace"
                              , content = Nothing
                              }
                          , Definition
                              { name =
                                  mconcat
                                    [ Code "-o"
                                    , ", "
                                    , Code "--output"
                                    , "="
                                    , Variable "FILE"
                                    ]
                              , description = Line ["Output to", Variable "FILE"]
                              , content = Nothing
                              }
                          , Definition
                              { name = Variable "INPUT"
                              , description = "Source input"
                              , content = Nothing
                              }
                          ]
                      ]
                  }
              , Section
                  { name = "ENVIRONMENT"
                  , content =
                      [ Definitions
                          [ Definition
                              { name = Code "HOME"
                              , description = "User's HOME directory"
                              , content =
                                  Just
                                    [ "Some further details:"
                                    , Definitions
                                        [ Definition
                                            { name = "foo"
                                            , description = "The foo"
                                            , content = Nothing
                                            }
                                        , Definition
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
              , Section
                  { name = "SEE ALSO"
                  , content =
                      [ Groups
                          [ Lines
                              [ Line
                                  [ mconcat $
                                      intersperse
                                        (Raw ", ")
                                        [ Ref $ ManRef "markdown" ManSection7
                                        , Ref $ ManRef "roff" ManSection7
                                        ]
                                  ]
                              ]
                          ]
                      ]
                  }
              ]
          }

ronnGolden :: Ronn -> Golden Text
ronnGolden ronn =
  Golden
    { output = ronnToText ronn
    , encodePretty = unpack
    , writeToFile = T.writeFile
    , readFromFile = T.readFile
    , goldenFile = "../doc" </> ronnFilePath ronn
    , actualFile = Nothing
    , failFirstTime = False
    }
