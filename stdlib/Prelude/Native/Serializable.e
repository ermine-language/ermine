module Native.Serializable where

import Native.Object

foreign
  data "java.io.Serializable" Serializable
  subtype SerializableObject : Serializable -> Object
