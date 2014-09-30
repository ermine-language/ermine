module Native.Either where

{- ^ The Scala Either, and conversion with Ermine Either. -}

import Either

private foreign
  data "scala.util.Left$" LeftModule
  value "scala.util.Left$" "MODULE$" leftModule : LeftModule
  method "apply" left# : LeftModule -> a -> Either# a b
  data "scala.util.Right$" RightModule
  value "scala.util.Right$" "MODULE$" rightModule : RightModule
  method "apply" right# : RightModule -> b -> Either# a b

toEither# : Either a b -> Either# a b
toEither# (Left x) = left# leftModule x
toEither# (Right x) = right# rightModule x

{-
builtin
  foreign data "scala.Either" Either# (a: *) (b: *)
-}
