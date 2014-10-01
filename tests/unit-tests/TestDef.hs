module TestDef where

import Control.Applicative
import Data.Traversable
import System.FilePath.Find
import Test.Framework.Providers.API as API
import Test.Framework.Providers.HUnit
import Test.HUnit

data TestDef = TestDef 
  { name            :: String
  , dir             :: FilePath
  , inputFileFormat :: String
  , run             :: FilePath -> Assertion
 }

defToTest :: TestDef -> IO API.Test
defToTest def = 
  testGroup (name def) . fmap mkTest <$> testFiles (inputFileFormat def) where
    testFiles rgx = 
      find always (fileType ==? RegularFile &&? fileName ~~? rgx) (dir def)
    mkTest inputFile = testCase inputFile $ run def inputFile 

