module Relation.RTree where

import Relation
import Relation.Row
import Relation.Predicate as P
import Relation.Op using prim
import Relation.UnifyFields
import Syntax.Relation
import Function

data RTree (r:row) (nid:row) (parent:row) =
  RTree (Predicate r) (Field nid Int) (Field parent Int)

rtree : Predicate r
     -> Field id Int
     -> Field parent Int
     -> RTree r id parent
rtree rootP i parent = RTree rootP i parent

rtreeAt : Field id Int -> Field parent Int -> Int -> RTree id id parent
rtreeAt i p nid = RTree (i ==_P (prim nid)) i p

-- level1 : (r <- (r1,r2), r1 <- (f,i,p,t)) => RTree f i p -> [..r] -> [..r]
-- level1 : (Intersects p r, Intersects nodeid r, Intersects parentid r)
--       => RTree p nodeid parentid -> [r] -> [r]
level1 (RTree f i p) r =
  [| f |] r # {i} |> [| p <- i |] |> join r

leaves (RTree f i p) r = leafRows p i r

root : r <- (f,t) => RTree f id parent -> [..r] -> [..r]
root (RTree f _ _) r = [| f |] r

