module Relation.Row where

import Eq
import List as List
import Syntax.List
import Function
import Field
import List.Util
import Ord
import Pair using fst
import Prim
import Native.List using toList#; fromList#
import Constraint
import Native.Pair
import Native.TraversableColumns as TC
import String as S

empty_Bracket = empty_Bracket_List
cons_Bracket = cons_Bracket_List

data Row (r:row) = Row (List (String, PrimT))

empty : Row (| |)
empty = Row []

single : Field r a -> Row r
single f = Row [(fieldName f, prim# $ fieldType f)]

append : forall (r1:row) (r2:row) (r:row) . r <- (r1,r2) => Row r1 -> Row r2 -> Row r
append (Row a) (Row b) = Row (a ++_List b)

snoc : forall (r1:row) (r2:row) (r:row) a . r <- (r1,r2) => Row r1 -> Field r2 a -> Row r
snoc r f = append r (single f)

minus : forall t r s. t <- (r, s) => Row t -> Row r -> Row s
minus (Row t) (Row r) = Row $
  filter_List ((x, _) -> every_List ((!=) x . fst) r) t

intersection (Row r1) (Row r2) =
  let r2names = map_List_List fst r2
  in Row $
    filter_List ((x, _) -> contains_List x r2names) r1

snoc_Brace = snoc
single_Brace = single

project : forall (r1:row) (r:row) . (Has r r1, RelationalComb rel)
       => Row r1 -> rel r -> rel r1
project (Row r) rel = project# (toList# (map toPair# r)) rel

except : forall (r1:row) (r2:row) (r:row) . (r <- (r1,r2), RelationalComb rel)
      => Row r1 -> rel r -> rel r2
except (Row r) rel = except# (toList# (map toPair# r)) rel

projectT : r <- (h,t) => Row h -> {..r} -> {..h}
projectT (Row r) tup = projectT# (toList# (map fst r)) tup

exceptT : r <- (h,t) => Row h -> {..r} -> {..t}
exceptT (Row r) tup = exceptT# (toList# (map fst r)) tup

spanT : r <- (h,t) => Row h -> {..r} -> ({..h}, {..t})
spanT r tup = (projectT r tup, exceptT r tup)

-- | For internal sharing; do not use.
rowUsed# : TraversableColumns#_TC r -> Row r
rowUsed# = Row . distinct (contramap fst ord_S) . map fromPair#
         . fromList# . rowUsed#_TC

infixl 5 # -# !*
(#) = flip project
(-#) = flip except
(!*) = flip projectT
