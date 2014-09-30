module Native.Bool where

import Bool

foreign
  data "java.lang.Boolean" Bool#
  value "java.lang.Boolean" "FALSE" false# : Bool#
  value "java.lang.Boolean" "TRUE" true# : Bool#

toBool# : Bool -> Bool#
toBool# False = false#
toBool# True = true#
