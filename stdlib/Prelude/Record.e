module Record where

import Native.Record
import Native.Ord
import Function
import Ord
import Relation using rheader; relation
import Relation.Row

type Record a = {..a}

appendR : c <- (a,b) => {..a} -> {..b} -> {..c}
appendR = appendRec#

infixr 5 ++
(++) = appendR

-- | Unlike recordOrd in Relation.Sort, this just picks an arbitrary
-- sort.
anyRecordOrd : Ord {..r}
anyRecordOrd = contramap (scalaRecord# . record#) anyRecordOrd#

header : {..r} -> Row r
header t = rheader (relation [t])
