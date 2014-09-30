module StringManip where

{- Here are some extra string manipulation functions, and examples of
working with strings in Ermine. -}

import Bool
import Control.Functor
import List
import Ord
import Function
import Native.List

private foreign
  data "java.lang.CharSequence" CharSequence#
  data "scala.collection.immutable.StringOps" ScalaStringOps#
  data "scala.util.matching.Regex" Regex#
  data "scala.util.matching.Regex$MatchIterator" MatchIterator#
  subtype StringIsCS : String -> CharSequence#
  constructor toStringOps : String -> ScalaStringOps#
  method "r" regex# : ScalaStringOps# -> Regex#
  method "findAllIn" srFindAllIn : Regex# -> CharSequence# -> MatchIterator#
  method "toList" miToList# : MatchIterator# -> List# String

foreign
  function "java.lang.Integer" "parseInt" parseInt : String -> Int

-- | Find all full matches of a regex in a string.
allMatches : String             -- ^ Regex.
          -> String             -- ^ String to search.
          -> List String
allMatches r = fromList# . miToList#
             . (srFindAllIn . regex# . toStringOps $ r) . StringIsCS

{-
>> allMatches  "\\d+" "1 2 3 4"
res0 : List String = ["1","2","3","4"]
-}

