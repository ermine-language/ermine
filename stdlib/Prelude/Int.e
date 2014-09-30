module Int where

foreign
  function "java.lang.Integer" "highestOneBit" highestOneBit : Int -> Int
  function "java.lang.Integer" "lowestOneBit" lowestoneBit : Int -> Int
  function "java.lang.Integer" "numberOfLeadingZeros" numberOfLeadingZeros : Int -> Int
  function "java.lang.Integer" "rotateLeft" rotateLeft : Int -> Int -> Int
  function "java.lang.Integer" "rotateRight" rotateRight : Int -> Int -> Int
  function "java.lang.Integer" "signum" signum : Int -> Int
  function "java.lang.Integer" "reverse" reverseBits : Int -> Int
  function "java.lang.Integer" "reverseBytes" reverseBytes : Int -> Int
  function "java.lang.Integer" "toString" radixString : Int -> Int -> String
  function "java.lang.Integer" "toHexString" hexString : Int -> String
  function "java.lang.Integer" "toOctalString" octalString : Int -> String
  function "java.lang.Integer" "toBinaryString" binaryString : Int -> String

-- the contents of this module are mostly provided by builtin primitives from Lib
