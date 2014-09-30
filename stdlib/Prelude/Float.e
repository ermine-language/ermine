module Float where

foreign
  function "java.lang.Math" "nextAfter"     nextAfter     : Float -> Double -> Float
  function "java.lang.Math" "copySign"      copySign      : Float -> Float -> Float
  function "java.lang.Math" "getExponent"   getExponent   : Float -> Int
  function "java.lang.Math" "nextUp"        nextUp        : Float -> Float
  function "java.lang.Math" "scalb"         scalb         : Float -> Int -> Float
  function "java.lang.Math" "signum"        signum        : Float -> Float
  function "java.lang.Math" "abs"           abs           : Float -> Float
  function "java.lang.Math" "ulp"           ulp           : Float -> Float
  value "java.lang.Float" "NaN" naN : Float
  value "java.lang.Float" "POSITIVE_INFINITY" infinity : Float
  value "java.lang.Float" "NEGATIVE_INFINITY" minfinity : Float
  value "java.lang.Float" "MIN_VALUE" minPositive : Float
  value "java.lang.Float" "MAX_VALUE" maxPositive : Float
  value "java.lang.Float" "MIN_EXPONENT" minExponent : Int
  value "java.lang.Float" "MAX_EXPONENT" maxExponent : Int
  value "java.lang.Float" "MIN_NORMAL" minNormal : Float
  function "java.lang.Float" "isNaN" isNaN : Float -> Bool
  function "java.lang.Float" "isInfinite" isInfinite : Float -> Bool
