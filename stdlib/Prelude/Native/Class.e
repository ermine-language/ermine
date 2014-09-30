module Native.Class where

foreign
  data "java.lang.Class" Class
  method "getName"          className : Class -> String
  method "getSimpleName"    simpleClassName : Class -> String
  method "isInstance"       isInstance : Class -> a -> Bool
  method "getClass"         getClass: a -> Class
  method "isAssignableFrom" isAssignableFrom: Class -> Class -> Bool
  method "asSubclass"       asSubclass : Class -> Class -> Class
  function "java.lang.Class" "forName" classForName: String -> IO Class

