module Native.Throwable where

import Bool
import Native.Object
import Native.Serializable

import Type.Cast
throwableObject       = Cast Throwable (x -> case x of Throwable y -> Just y; _ -> Nothing)
throwableSerializable = Cast ThrowableSerializable (x -> case x of ThrowableSerializable y -> Just y; _ -> Nothing)

getCause : Throwable -> Maybe Throwable
getCause t = liftNull (getCause' t)

foreign
  method "getMessage" getMessage : Throwable -> String
  method "getCause" getCause' : Throwable -> Throwable
  constructor throwable : String -> Throwable
  constructor throwableWithCause : String -> Throwable -> Throwable
  subtype ThrowableSerializable : Throwable -> Serializable
  subtype Throwable : Throwable -> Object

-- builtin bottom : Throwable -> a
