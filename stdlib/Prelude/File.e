module File where

import IO
import Syntax.IO
import Function
import String

type FilePath = String
type URL = String
type Charset = String

readFile : FilePath -> IO String
readFile path = sourceToString# $ fromFile# path "UTF-8"

readURL  : URL -> IO String
readURL  path = sourceToString# $ fromURL#  path "UTF-8"

fileLines : FilePath -> IO (List String)
fileLines path = map (lines . trim) $ readFile path

private
  withSource# : (Source# -> a) -> Source# -> a
  withSource# f s = let res = f s in let x = close# s in traceShow res

  sourceToString# : IO Source# -> IO String
  sourceToString# = map $ withSource# mkString#

foreign
  data "java.io.File" File
  constructor file# : FilePath -> File

private foreign
  data "scala.io.Source" Source#
  function "scala.io.Source" "fromFile" fromFile# : FilePath -> Charset -> IO Source#
  function "scala.io.Source" "fromURL"  fromURL#  : URL      -> Charset -> IO Source#
  method "mkString" mkString# : Source# -> String
  method "close" close# : Source# -> ()
