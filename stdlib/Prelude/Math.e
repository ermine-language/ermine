module Math where

import Primitive
import Bool

foreign
  value "java.lang.Math" "PI" pi : Double
  value "java.lang.Math" "PI" π : Double
  -- value "java.lang.Math" "E" e : Double
  function "java.lang.Math" "toDegrees" toDegrees : Double -> Double
  function "java.lang.Math" "toRadians" toRadians : Double -> Double
  function "java.lang.Math" "hypot"         hypot         : Double -> Double -> Double
  function "java.lang.Math" "IEEEremainder" ieeeRemainder : Double -> Double -> Double
  function "java.lang.Math" "copySign"      copySign      : Double -> Double -> Double
  function "java.lang.Math" "atan2"         atan2         : Double -> Double -> Double
  function "java.lang.Math" "getExponent"   getExponent   : Double -> Int
  function "java.lang.Math" "ceil"   ceil    : Double -> Double
  function "java.lang.Math" "floor"  floor   : Double -> Double
  function "java.lang.Math" "rint"   rint    : Double -> Double
  function "java.lang.Math" "signum" signum  : Double -> Double
  function "java.lang.Math" "asin"   asin    : Double -> Double
  function "java.lang.Math" "acos"   acos    : Double -> Double
  function "java.lang.Math" "atan"   atan    : Double -> Double
  function "java.lang.Math" "sin"    sin     : Double -> Double
  function "java.lang.Math" "cos"    cos     : Double -> Double
  function "java.lang.Math" "tan"    tan     : Double -> Double
  function "java.lang.Math" "sinh"   sinh    : Double -> Double
  function "java.lang.Math" "cosh"   cosh    : Double -> Double
  function "java.lang.Math" "tanh"   tanh    : Double -> Double
  function "java.lang.Math" "cbrt"   cbrt    : Double -> Double
  function "java.lang.Math" "sqrt"   sqrt    : Double -> Double
  function "java.lang.Math" "log"    log     : Double -> Double
  function "java.lang.Math" "log10"  log10   : Double -> Double
  function "java.lang.Math" "log1p"  log1p   : Double -> Double
  function "java.lang.Math" "exp"    exp     : Double -> Double
  function "java.lang.Math" "expm1"  expm1   : Double -> Double
  function "java.lang.Math" "round" round    : Double -> Long
  function "java.lang.Math" "nextAfter" nextAfter : Double -> Double -> Double
  function "java.lang.Math" "scalb"  scalb : Double -> Int -> Double
  function "java.lang.Math" "random" random : IO Double
  function "java.lang.Math" "ulp" ulp : Double -> Double
  function "java.lang.Math" "nextUp" nextUp  : Double -> Double

  value "java.lang.Double" "NaN" naN : Double
  value "java.lang.Double" "POSITIVE_INFINITY" infinity : Double
  value "java.lang.Double" "NEGATIVE_INFINITY" minfinity : Double
  value "java.lang.Double" "MAX_VALUE" maxPositive : Double
  value "java.lang.Double" "MIN_VALUE" minPositive : Double
  value "java.lang.Double" "MIN_EXPONENT" minExponent : Int
  value "java.lang.Double" "MAX_EXPONENT" maxExponent : Int
  value "java.lang.Double" "MIN_NORMAL" minNormal : Double
  function "java.lang.Double" "isNaN" isNaN : Double -> Bool
  function "java.lang.Double" "isInfinite" isInfinite : Double -> Bool

ensureNonnegativeWeight : (Double, Double) -> (Double, Double)
ensureNonnegativeWeight (v, w) = if (w < 0.0) (-v, -w) (v, w)

weightedSum : Double -> (Double, Double) -> Double
weightedSum s (v, w) = s + v * w

harmonicQuotientSum : Double -> (Double, Double) -> Double
harmonicQuotientSum s (v, w) = s + w / v
