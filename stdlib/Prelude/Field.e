module Field where

import Function
import Maybe
import Nullable
import GUID
import IO.Unsafe

export Field.Type

getF : r <- (h,t) => Field h a -> {..r} -> a
getF f t = t ! f

getFieldOr : r <- (h,t) => a -> Field h (Nullable a) -> {..r} -> a
getFieldOr default f r = orElse default ' toMaybe (r ! f)

getF2 f1 f2 a = (a ! f1, a ! f2)

-- existentialF : String -> Prim a -> exists r . Field (|r|) a
data EField a = forall r . EField (Field r a)

existentialF : String -> Prim a -> EField a
existentialF s p = EField (existentialF# s p)

ecopyF : Field r a -> EField a
ecopyF f = let i = guidString ' unsafePerformIO guid
           in existentialF i (fieldType f)

withFieldCopy : Field r a -> (forall r . Field r a -> b) -> b
withFieldCopy f (cont : some a b . forall r . Field r a -> b) =
  case ecopyF f of EField f' -> cont f'

modify : r <- (h, t) => Field h a -> (a -> a) -> {..r} -> {..r}
modify fld g rec = cons fld (g (rec ! fld)) (rec \ fld)

update : r <- (h, t) => Field h a -> a -> {..r} -> {..r}
update f v rec = modify f (const v) rec

field Count : Int

private
-- builtin
--   infixr 5 . \
--   cons : t <- (r, s) => Field r a -> a -> {..s} -> {..t}
--   (!) : t <- (r, s) => {..t} -> Field r a -> a
--   (\) : t <- (r, s) => {..t} -> Field r a -> {..s}
--   fieldName : Field r a -> String
--   fieldType : Field r a -> Prim a
--   existentialF# : String -> Prim a -> Field r a
