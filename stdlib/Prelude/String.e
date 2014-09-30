module String where

import Bool using if
import Control.Monoid
import Function
import Maybe using maybe
import Num
import Native.List
import List as List
import Ord as O
import Native.Object

stringMonoid = Monoid "" (++)

-- | Return the remainder of the string after `n` characters, or "" if
-- `n` is greater than that length.
drop : Int -> String -> String
drop n s = if (n <= length s) (drop# s n) ""

-- | Take the first `n` characters, or the whole string if `n` is as
-- large or larger.
take : Int -> String -> String
take n s = if (n < 0) ""
           $ if (n <= length s) (substring# s 0 n) s

-- | Split a string along some character delimiter
split : Char -> String -> List String
split c s = fromList# $ split# s c

unsplit : Char -> List String -> String
unsplit c = maybe "" id . foldr1_List (x -> y -> x ++ (toString c) ++ y)

replace : Char -> Char -> String -> String
replace old new = unsplit new . split old

-- | Split a string along spaces into words -
-- e.g. "foo bar baz" -> ["foo", "bar", "baz"]
words = split ' '

spaced = unwords

-- | Inverse of words.
unwords : List String -> String
unwords = maybe "" id . foldr1_List (x -> y -> x ++ " " ++ y)

concat : List String -> String
concat = foldr_List (++) ""

-- | Split a string along newlines
lines = split '\n'

substring : Int -> Int -> String -> String
substring i j s = substring# s i j

substringFrom : Int -> String -> String
substringFrom i s = substring# s i (length s)

substringTo : Int -> String -> String
substringTo i s = substring# s 0 i

ord = fromLess_O (<)

replaceAll : String -> String -> String -> String
replaceAll searchRegex replaceRegex str = replaceAll# str searchRegex replaceRegex

{- Converts a CamelCase word to a series of space-separated words,
   handling acronyms appropriately.

   Examples:
      "lowercase" => "lowercase"
      "Class" => "Class"
      "MyClass" => "My Class"
      "HTML" => "HTML"
      "PDFLoader" => "PDF Loader"
      "AString" => "A String"
      "SimpleXMLParser" => "Simple XML Parser"
      "GL11Version" => "GL 11 Version"
      "99Bottles" => "99 Bottles"
      "May5" => "May 5"
      "BFG9000" => "BFG 9000"
-}
splitCamelCase s =
  let findAcronyms = "(?<=[A-Z])(?=[A-Z][a-z])"
      camelCasedWord = "(?<=[^A-Z])(?=[A-Z])"
      nonLetter = "(?<=[A-Za-z])(?=[^A-Za-z])"
      searchRegex = concat $ intersperse_List "|" [findAcronyms, camelCasedWord, nonLetter]_List
      replaceRegex = " " -- these regexes match zero characters due to lookahead/behind magic
  in replaceAll searchRegex replaceRegex s

foreign
  method "indexOf" indexOf : String -> String -> Int
  method "trim"    trim    : String -> String

private foreign
  method "substring" drop#      : String -> Int -> String
  method "substring" substring# : String -> Int -> Int -> String
  method "replaceAll" replaceAll# : String -> String -> String -> String

-- builtin
--   infixr 5 ++
--   (++) : String -> String -> String
--   length : String -> Int
--   split# : String -> Char -> List# String
