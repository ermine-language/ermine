module IO.Unsafe where

import Either
import Native.Throwable
import Function

runIO : IO a -> (a -> r) -> (forall i. (i -> r) -> (Throwable -> r) -> FFI i -> r) -> (Throwable -> r) -> r
runIO (IO f) = f

unsafePerformIO : IO a -> a
unsafePerformIO f = runIO f id eval raise

unsafeFFI : FFI a -> Either Throwable a
unsafeFFI m = case eval Right Left m of
  Left e@(RuntimeException _) -> case getCause e of
    Nothing     -> Left e
    Just cause  -> Left cause
  Left e  -> raise e
  Right a -> Right a

private foreign
  data "java.lang.RuntimeException" RuntimeException
  subtype RuntimeException : RuntimeException -> Throwable

-- native eval : forall a r. (a -> r) -> (Throwable -> r) -> FFI a -> r
