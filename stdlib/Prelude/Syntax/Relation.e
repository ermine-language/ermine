module Syntax.Relation where

{- We do some tricks in DMTL parsing, *before* typing, for making
relation transformations easier to write.

    doSomeStuff = rename x y
                . filter (col x >_Pred prim 100)
                . combine (col z +_Op prim 50) x

can be written as

    simpler = [| x = z + 50, x > 100, y <- x |]

    doSomeStuff, simpler : forall (r: ρ) (a: ρ).
           (exists (o: ρ) (t: ρ).
            a <- ((|y|), r),
            t <- ((|x|), r),
            r <- ((|z|), o))
           => [..r] -> [..a]

Transformations apply left-to-right.

The usability of columns depends on mentioning them in the list before
=>; we mentioned `z` as our input constraint above, so we would know
to use `col` before injecting it into the `combine`.

The Syntax.RelationTest unit-test module has several more examples of
what expressions are allowed.

Using disallowed expressions will yield a parser or type error, but
these can often be worked around by using `col`, `prim`, or the
op/predicate functions explicitly. -}

import Relation
import Native.Relation
import List

-- enable [| y <- x, x = y + 42, x < 3 |] syntax
import Function
import Relation.Predicate using {}
import Relation.Op using {}

infixl 5 **
infixl 5 &
infixl 5 +++

(**) = join
(&) r t = join r (relation [t])
(+++) = union