module Native.Record where

import Native.Ord
import Ord

-- builtin
--   data Record#
--   data ScalaRecord#
--   record# :: {..a} -> Record#
--   unsafeRecordIn# :: Record# -> {..a}
--   scalaRecord# : Record# -> ScalaRecord#
--   scalaRecordIn# : ScalaRecord# -> Record#

anyRecordOrd# : Ord ScalaRecord#
anyRecordOrd# = fromOrd# anyRecordOrd##

private foreign
  function "com.clarifi.reporting.package" "RecordOrder"
    anyRecordOrd## : Ord# ScalaRecord#
