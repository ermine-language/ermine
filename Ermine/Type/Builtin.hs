module Ermine.Type.Builtin
  (
  -- * Builtin Types
  -- ** Numerics
    int
  , float, double
  -- ** Containers
  , list
  , maybe_
  -- ** Classes
  , functor
  -- * Utilities
  , builtin, builtin_
  , apps
  ) where

import Data.ByteString.Char8 hiding (foldl)
import Data.Void
import Ermine.Fun
import Ermine.Global
import Ermine.Kind
import Ermine.Type

builtin :: Typical t => Schema Void -> String -> t
builtin s n = con (global Idfix (pack "ermine") (pack "Prelude") (pack n)) s

builtin_ :: Typical t => String -> t
builtin_  = builtin star

int, double, float :: Typical t => t
int    = builtin_ "Int"
double = builtin_ "Double"
float  = builtin_ "Float"

list :: Typical t => t
list = builtin (star ~> star) "List"

maybe_ :: Typical t => t
maybe_ = builtin (star ~> star) "Maybe"

functor :: Typical t => t
functor = builtin ((star ~> star) ~> constraint) "Functor"

apps :: Type k t -> [Type k t] -> Type k t
apps = foldl App
