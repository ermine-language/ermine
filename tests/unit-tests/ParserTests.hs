module ParserTests
  ( ermineParserTests
  , stdLibDir
  ) where

import Control.Applicative
import Data.Traversable
import System.FilePath ((</>))
import Test.HUnit

import TestDef

stdLibDir = "stdlib" </> "Prelude"

ermineParserTests  = TestDef {
   name            = "Ermine Parser Tests"
 , dir             = stdLibDir
 , inputFileFormat = "*.e"
 , run             = \ermineFile -> do 
     ermineCode <- readFile ermineFile
     putStrLn ermineFile
     --TODO: put a real assertion here
     --parse ermineCode @?= someExpectedValueHere
}

