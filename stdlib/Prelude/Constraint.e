module Constraint where

infix type 9 |

type Has a b = exists c. a <- (b, c)
type (|) a b = exists c. c <- (a, b)
type RUnion2 t r s = exists ro so rs. t <- (ro, so, rs), r <- (ro, rs), s <- (so, rs)
type RUnion3 v r s t = exists ro so to rs rt st rst.
                       v <- (ro, so, to, rs, rt, st, rst),
                       r <- (ro, rs, rt, rst),
                       s <- (so, rs, st, rst),
                       t <- (to, rt, st, rst)
type Intersection f a b = forall c. (exists ap bp. a <- (ap, c), b <- (bp, c), ap | bp) => f c
type Union f a b = forall d. (exists ap c. a <- (ap, c), Has c b, d <- (ap, b)) => f d
