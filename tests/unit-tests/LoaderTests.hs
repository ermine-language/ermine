{-# LANGUAGE OverloadedStrings #-}
module LoaderTests (ermineLoaderTests) where

import Control.Lens
import Control.Monad.Trans.Except
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Ermine.Loader.Core
import Ermine.Loader.Filesystem
import System.FilePath ((</>))
import Test.HUnit

import TestDef
import ParserTests (stdLibDir)

stdLibLoader :: Loader Freshness (ExceptT LoadRefusal IO) Text Text
stdLibLoader = filesystemLoader stdLibDir "e"

loadsAFile = "loads a file" ~: do lr <- runExceptT (stdLibLoader ^. load) "Byte"
                                  fraw <- TIO.readFile (stdLibDir </> "Byte.e")
                                  Right fraw ~=? fmap snd lr

ermineLoaderTests = test [loadsAFile]
