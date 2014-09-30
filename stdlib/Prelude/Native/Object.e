module Native.Object where

import Bool

foreign
  data "java.lang.Object" Object
  method "toString" toString : a -> String

-- nulls
liftNull : a -> Maybe a
liftNull a = if (isNull a) Nothing (Just a)

-- structural equality
-- builtin (==) :: a -> a -> Bool

-- referential equality
-- builtin (===) :: a -> a -> Bool
