{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

-- haddock stuff
import Distribution.Package ( Package (..), packageName )
import Distribution.Simple ( defaultMainWithHooks, UserHooks(..), simpleUserHooks )
import Distribution.Simple.Setup (Flag (..), HaddockFlags, haddockDistPref)
import Distribution.Simple.Utils (copyFiles)
import Distribution.Verbosity (normal)
import Distribution.Text ( display )
import System.FilePath ( (</>) )

import Distribution.Extra.Doctest ( doctestsUserHooks )

haddockOutputDir :: Package p => HaddockFlags -> p -> FilePath
haddockOutputDir flags pkg = destDir where
  baseDir = case haddockDistPref flags of
    NoFlag -> "."
    Flag x -> x
  destDir = baseDir </> "doc" </> "html" </> display (packageName pkg)

main :: IO ()
main = defaultMainWithHooks duh
  { postHaddock = \args flags pkg lbi -> do
     copyFiles normal (haddockOutputDir flags pkg) [("images","Hierarchy.png")]
     postHaddock duh args flags pkg lbi
  }
  where
    duh = doctestsUserHooks "doctests"
