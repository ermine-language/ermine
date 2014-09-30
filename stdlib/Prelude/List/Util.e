module List.Util where

import Map as M
import Ord
import Primitive
import Function
import List
import Bool
import Pair
import Syntax.List
import Num
import Eq
import Maybe
import Math
import String as S

-- | Left-biased, O(n log n) duplicate elimination.
distinct : Ord a -> List a -> List a
distinct o xs =
  let next (_, []) = Nothing
      next (seen, (h :: t)) =
        if (member_M h seen) (next (seen, t))
                             (Just (h, (insert_M h () seen, t)))
   in unfoldr next (empty_M o, xs)

sort : Ord a -> List a -> List a
sort o = map fst . toAssocList_M . fromAssocList_M o
       . map (a -> (a, ()))

median : List Double -> Maybe Double
median [] = Nothing
median vs = let
    sorted = sort primOrd vs
    l = length vs
    midPoint = l / 2
    isEven = midPoint * 2 == l
    m : Double
    m = getJust ' "Invalid index" ' at midPoint sorted
  in if isEven
    (Just ((m + (getJust "Invalid index" ' at (midPoint-1) sorted)) / 2.0))
    (Just m)

weightedMean : List (Double, Double) -> Maybe Double
weightedMean ts = let
    ts' = ensureNonnegativeWeights ts
    sumValues  = ts' |> foldl weightedSum 0.0
    sumWeights = ts' |> map snd |> foldl (+) 0.0
  in if (sumWeights == 0.0)
        Nothing
        (Just (sumValues / sumWeights))

weightedHarmonicMean : List (Double, Double) -> Maybe Double
weightedHarmonicMean ts = let
    ts' = ensureNonnegativeWeights ts
    sumWeights = ts' |> map snd |> foldl (+) 0.0
    sumQuotient = ts' |> foldl harmonicQuotientSum 0.0
  in if (sumQuotient == 0.0)
        Nothing
        (Just (sumWeights / sumQuotient))

ensureNonnegativeWeights : List (Double, Double) -> List (Double, Double)
ensureNonnegativeWeights ts = map ensureNonnegativeWeight ts

