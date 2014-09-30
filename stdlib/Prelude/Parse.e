module Parse where

import IO.Unsafe
import Control.Monoid
import Control.Monad
import Native.Throwable
import Native.Object
import Native.Class
import Either
import Function
import String
import Error
import Maybe

private numberFormat : FFI a -> Maybe a
numberFormat m = case unsafeFFI m of
  Left e@(NumberFormatException _) -> Nothing
  Left e  -> raise e
  Right n -> Just n

private foreign
  data "java.lang.NumberFormatException" NumberFormatException
  subtype NumberFormatException : NumberFormatException -> Throwable
  function "java.lang.Integer" "parseInt"    parseInt#    : String -> Int -> FFI Int
  function "java.lang.Byte"    "parseByte"   parseByte#   : String -> Int -> FFI Byte
  function "java.lang.Long"    "parseLong"   parseLong#   : String -> Int -> FFI Long
  function "java.lang.Short"   "parseShort"  parseShort#  : String -> Int -> FFI Short
  function "java.lang.Float"   "parseFloat"  parseFloat#  : String -> FFI Float
  function "java.lang.Double"  "parseDouble" parseDouble# : String -> FFI Double

parseInt   radix s = numberFormat (parseInt# s radix)
parseByte  radix s = numberFormat (parseByte# s radix)
parseShort radix s = numberFormat (parseShort# s radix)
parseLong  radix s = numberFormat (parseLong# s radix)

parseDouble s = numberFormat (parseDouble# s)
parseFloat s = numberFormat (parseFloat# s)

parseBool "true"  = Just True
parseBool "false" = Just False
parseBool _       = Nothing
