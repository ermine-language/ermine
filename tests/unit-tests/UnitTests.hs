module Main where

import Data.Traversable
import Test.Framework.Runners.Console
import Test.Framework.Providers.API as API
import Test.Framework.Providers.HUnit
import Test.HUnit

import TestDef
import qualified LoaderTests
import qualified ParserTests

main :: IO ()
main = do
  g <- fileBasedTestGroup
  defaultMain
    [ g
    , ParserTests.main
    , LoaderTests.main
    ]

fileBasedTestGroup :: IO API.Test
fileBasedTestGroup = do
  ts <- traverse defToTest fileBasedTests
  return $ testGroup "File Based Tests" ts

-- add more tesst here!
fileBasedTests :: [TestDef]
fileBasedTests = [ParserTests.ermineParserTests]

