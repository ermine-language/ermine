module Date where

import Function
import Primitive
import Native
import Ord
import List as L
import Maybe as M
import String as S

-- builtin data "java.util.Date" Date

parseDate : String -> Maybe Date
parseDate = fromMaybe# . parseDate#

foreign
  constructor now : IO Date
  constructor ddmmyy : Int -> Int -> Int -> Date
  constructor dateFromLong : Long -> Date
  -- constructor dateFromString : String -> Date
  function "com.clarifi.reporting.PrimExprs" "parseDate" parseDate# : String -> Maybe# Date
  function "com.clarifi.reporting.PrimExprs" "formatDate" formatDate : Date -> String
  function "com.clarifi.reporting.PrimExprs" "formatMonthYear" formatMonthYear : Date -> String
  function "com.clarifi.reporting.PrimExprs" "formatYear" formatYear : Date -> String
  function "com.clarifi.reporting.PrimExprs" "formatMonth" formatMonth : Date -> String

  method "getTime" getTime       : Date -> Long
  method "getDate" getDate       : Date -> Int
  method "getMonth" getMonth     : Date -> Int
  method "getYear" getYear       : Date -> Int
  method "getHours" getHours     : Date -> Int
  method "getMinutes" getMinutes : Date -> Int
  method "before" before : Date -> Date -> Bool

formatExcelDate d =
  spaced_S ' [
    toString . getJust_M "unknown month" ' at_L (getMonth d) shortMonthNames,
    toString ' getDate d
  ]_L

monthNames : List String
monthNames =
  ["January", "February", "March", "April", "May", "June",
   "July", "August", "September", "October", "November", "December"]_L

shortMonthNames : List String
shortMonthNames =
  ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]_L

quarterNames : List String
quarterNames = ["Q1", "Q2", "Q3", "Q4"]_L

quarter : Date -> Int
quarter d = getMonth d / 4 + 1

formatQuarter : Date -> String
formatQuarter d = orElse_M "Unknown" (at_L (quarter d) quarterNames)

-- | `incrementDate 5 days d`
incrementDate : Int -> TimeUnit -> Date -> Date
incrementDate n u d = incrementDate# u d n

decrementDate : Int -> TimeUnit -> Date -> Date
decrementDate n = incrementDate (-n)

-- date arithmetic
foreign
  data "com.clarifi.reporting.TimeUnit" TimeUnit
  function "com.clarifi.reporting.TimeUnits" "Day" days : TimeUnit
  function "com.clarifi.reporting.TimeUnits" "Week" weeks : TimeUnit
  function "com.clarifi.reporting.TimeUnits" "Month" months : TimeUnit
  function "com.clarifi.reporting.TimeUnits" "Year" years : TimeUnit

private foreign
  method "increment" incrementDate# : TimeUnit -> Date -> Int -> Date
