module Relation.Aggregate.Unsafe where

import Native
import Relation.Op.Type
import Relation.Aggregate.Type

foreign
  value "com.clarifi.reporting.AggFunc$Count$" "MODULE$"
      countModule : Aggregate (| |) a
  value "com.clarifi.reporting.AggFunc$Sum$" "MODULE$"
      sumModule : Function1 (Op r a) (Aggregate r a)
  value "com.clarifi.reporting.AggFunc$Avg$" "MODULE$"
      avgModule : Function1 (Op r a) (Aggregate r a)
  value "com.clarifi.reporting.AggFunc$Min$" "MODULE$"
      minModule : Function1 (Op r a) (Aggregate r a)
  value "com.clarifi.reporting.AggFunc$Max$" "MODULE$"
      maxModule : Function1 (Op r a) (Aggregate r a)
  value "com.clarifi.reporting.AggFunc$Stddev$" "MODULE$"
      stddevModule : Function1 (Op r a) (Aggregate r a)
  value "com.clarifi.reporting.AggFunc$Variance$" "MODULE$"
      varianceModule : Function1 (Op r a) (Aggregate r a)
