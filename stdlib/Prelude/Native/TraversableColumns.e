module Native.TraversableColumns where

import Function
import List.Util
import Native.List
import Native.Pair
import Ord
import String as S

foreign
  data "com.clarifi.reporting.TraversableColumns" TraversableColumns# (r: ρ)
  -- | Answer the Row schema used by a presentation.
  method "typedColumnReferencesList"
      rowUsed# : TraversableColumns# r -> List# (Pair# String PrimT)

-- | Answer unique columns referenced, in order.
columnsUsed# : TraversableColumns# r -> List String
columnsUsed# = distinct ord_S . fromList# . columnsUsed##

private foreign
  method "columnReferencesList"
      columnsUsed## : TraversableColumns# r -> List# String
