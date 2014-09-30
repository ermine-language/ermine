module Relation.UnifyFields where

import Relation
import Relation.Row

unify1 : forall (r:row) (r2:row) . (r <- (h,f,t), r2 <- (h,f2,t))
      => Field f1 a -> Field f2 a -> [..r] -> [..r2] -> [..r]
unify1 f1 f2 r r2 = join (rename f1 f2 (except {f2} r2)) r
