module Prelude where

export Eq
export Prim
export Field
export Pair
export Bool
export Function
export Error

export List
export Map as Map
export Vector as Vector
export Maybe
export Nullable as Nullable
export IO
export Either
export Control.Monad.Cont

export Math

export Num
export Primitive as Primitive
export Int as Int
export Byte as Byte
export Long as Long
export Short as Short
export Double as Double
export Float as Float
export String as String
export Date
export DateRange as DateRange

export Void

export Ord
export Ring

export Record as Record
export Constraint
export Relation.Predicate as Predicate
export Relation.Op as Op
export Relation.Row
export Relation.UnifyFields
export Relation
export Runners as Runners
export Scanners as Scanners

export Type.Eq as Type

-- export Control.Monad
export Syntax.Do
-- export Syntax.Monad
export Control.Comonad
export Control.Alt
export Control.Traversable
export Control.Functor

export Native.Object -- toString
export Native.Throwable -- bottom
export Native.Exception

export Parse

-- for testing, probably not for release
export Unsafe.Coerce
export IO.Unsafe
export Native

export Syntax.IO as IO
export Syntax.List as List
import List.Util as ListUtil

export Syntax.Maybe as Maybe
export Syntax.Either as Either
export Syntax.Relation as Relation

export DB

type Scanner = Scanner_Scanners
type Runner = Runner_Runners

formatDateRange = formatDateRange_DateRange
formatPeriod = formatPeriod_DateRange
formatPeriodOr = formatPeriodOr_DateRange

infixl 5 **
infixl 5 +++

primOrd = primOrd_Primitive
all = all_Predicate
filterAll = filter_Predicate . all
selectNulls = selectNulls_Predicate
selectNotNulls = selectNotNulls_Predicate
(**) = (**_Relation)
(+++) = (+++_Relation)
spaced = spaced_String
splitCamelCase = splitCamelCase_String
appendR = appendR_Record
map = map_List
distinct = distinct_ListUtil
sort = sort_ListUtil
median = median_ListUtil
weightedMean = weightedMean_ListUtil
weightedHarmonicMean = weightedHarmonicMean_ListUtil
vector = vector_Vector

headFieldOr ifMissing f l =
  maybeHead ifMissing (orElse ifMissing . toMaybe_Nullable . getF f) l
