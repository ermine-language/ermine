module DateRange where

import List as L
import Ord
import Eq
import Primitive
import Function
import StringManip
import Control.Ap
import Maybe hiding ord
import Date as D
import String as S
import Native using toString

-- | Order by start date, then end date.
ord : Ord (Date,Date)
ord = contramap ((a,b) -> [a,b]_L) (ord_L primOrd)

formatDateRange (start, stop) = unwords_S [formatDate_D start, "-", formatDate_D stop]_L

formatExcelPeriod (start, stop) =
  spaced_S [ formatExcelDate_D start, "to", formatExcelDate_D stop]_L

formatPeriod default d@(start, stop) =
  case (getYear_D stop - getYear_D start, abs (getMonth_D stop - getMonth_D start)) of
    (0, 1) -> formatMonthYear_D stop
    (1, 11) -> formatMonthYear_D stop -- Dec to Jan
    (0, 3) -> spaced_S [formatQuarter_D stop, formatYear_D stop]_L
    (1, 0) -> formatYear_D stop
    _ -> default d

formatPeriodOr default = formatPeriod (const default)

parseDateRange : String -> Maybe (Date,Date)
parseDateRange s = case allMatches "[^\\-]+" s of
  h :: h2 :: [] -> liftA2 maybeAp (,) (parseDate_D h) (parseDate_D h2)
  _ -> Nothing

ltStringDateRange : String -> String -> Bool
ltStringDateRange l r = case (parseDateRange l, parseDateRange r) of
  (Just ld, Just rd) -> lt ord ld rd
  _ -> False                    -- XXX may break on non-dateranges
