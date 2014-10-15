module Main where

import Test.Framework

import qualified LoaderTests

main :: IO ()
main = defaultMain
  [ LoaderTests.main
  ]
