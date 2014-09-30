module Syntax.Procedure where

{- This module provides support for the `procedure` syntax.  You
should not import it; the syntax is always available.

This syntax:

field field1: Int
field field2: String
field field3: Nullable Double
database "somedb"
  procedure "sqlprocname" blah.dbo.ermineProc 0
      : Int -> String -> Mem (| field1, field2, field3 |)

Yields something like:

ermineProc : Int -> String -> Mem (| field1, field2, field3 |)
ermineProc a b = unsafeCallStoredProcedure
                  "somedb" "sqlprocname" ["blah", "dbo"] 0
                  {field2, field1, field3}
                  [storedProcedureArg a, storedProcedureArg b]

where the brackets are fixed to be List syntax.
-}

import Either
import Field using fieldName; fieldType
import Function
import Native.Either
import Native.Function
import Native.List using toList#; type List#
import Native.Pair using toPair#; type Pair#
import Prim using prim#; primExpr#; type PrimExpr#
import Relation.Op
import Relation.Op.Unsafe using UnsafeOp; funcallModule
import Relation.Row
import Syntax.List using map
import Unsafe.Coerce

data ProcedureArg = PrimArg PrimExpr#
                  | forall r. RelArg [..r] String -- in-db table type name

data FunctionArg = forall r a. FunctionArg (Op r a)
functionArg = FunctionArg . asOp

storedProcedureArg : Primitive a => a -> ProcedureArg
storedProcedureArg = PrimArg . primExpr#

-- | Build an Op that calls the given function with scalar arguments,
-- yielding a scalar result.
unsafeCallScalarFunction : String -- ^ database context, or ""
                        -> String -- ^ function name
                        -> List String -- ^ name qualifiers
                        -> Prim a      -- ^ result type
                        -> List FunctionArg -- ^ positional arguments
                        -> Op r a      -- ^ result
unsafeCallScalarFunction dbc func qual ty args =
  funcallOp# func dbc (toList# qual)
             (toList# $ map ((FunctionArg a) -> UnsafeOp a) args)
             ty

-- | Build a stored procedure call taking a list of primitives and
-- relations as arguments, yielding a result in Mem.
unsafeCallStoredProcedure : String      -- ^ database context, or ""
                         -> String      -- ^ procedure name
                         -> List String       -- ^ name qualifiers
                         -> Row r             -- ^ result row-type, in order
                         -> List ProcedureArg -- ^ positional arguments
                         -> [..r]             -- ^ unsafe result type
unsafeCallStoredProcedure dbc proc qual rowty args =
  unsafeCallStoredProcedure# dbc proc (toList# qual)
      (rowInfo# rowty)
      (toList# $ map procArg args)

private
  funcallOp# = funcall5# funcallModule

  unsafeRel : forall r s. [..r] -> [..s]
  unsafeRel = unsafeCoerce

  rowInfo# : Row r -> List# (Pair# String PrimT)
  rowInfo# (Row xs) = toList# . map toPair# $ xs

  procArg : forall r. ProcedureArg -> Either# (Pair# String [..r]) PrimExpr#
  procArg (PrimArg a) = toEither# (Right a)
  procArg (RelArg r sty) = toEither# . Left . toPair# $ (sty, unsafeRel r)

{-
builtin
  unsafeCallStoredProcedure# : String -> String
                            -> List# String
                            -> List# (Pair# String PrimT)
                            -> List# (Either# (Pair# String [..!sk]) PrimExpr#)
                            -> [..r]
-}
