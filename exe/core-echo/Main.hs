{-# LANGUAGE TemplateHaskell #-}
module Main ( main ) where

import Control.Exception (catch, SomeException)
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as B
import Data.List (intersperse)
import Data.Monoid (mappend, mconcat)
import Ermine.Syntax.Module

main :: IO ()
main = do
       input <- B.readFile "core.in"
       putStrLn . showbytes $ input
       putStrLn . showbytes $ writeCore $ readCore input
       let r = B.writeFile "core.out" . writeCore $ readCore input
       catch r (writeFile "failure.txt" . mappend "failed: " . (show :: SomeException -> String))

readCore :: B.ByteString -> Module
readCore = Binary.decode

showbytes :: B.ByteString -> String
showbytes = mconcat . ("Bytes: ":) . intersperse ", " . map show . B.unpack

writeCore :: Module -> B.ByteString
writeCore = Binary.encode
