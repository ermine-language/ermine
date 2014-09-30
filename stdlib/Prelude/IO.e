module IO where

import Native.Throwable
import Native.Exception
import Native.Object
import Control.Monad
import Control.Monoid
import Control.Alt
import IO.Unsafe
import Function
import String

infixl 4 >>= <*>

private type K r = forall i. (i -> r) -> (Throwable -> r) -> FFI i -> r

ioFunctor = Functor ioMap
private
  ioMap f m = IO (kp -> runIO m (x -> kp (f x)))

ioAp = Ap ioReturn (<*>)
private
  ioReturn a = IO (kp _ _ -> kp a)
  (<*>) mf ma = mf >>= f -> ioMap f ma

ioMonad = Monad ioReturn (>>=)
private
  (>>=) m f = IO (kp (kf : some r. K r) ke -> runIO m (a -> runIO (f a) kp kf ke) kf ke)

-- throw : forall a. Throwable -> IO a
throw e = IO (_ _ ke -> ke e)

-- catch : forall a. IO a -> (Throwable -> IO a) -> IO a
catch m f = IO (kp (kf : some r. K r) ke -> runIO m kp kf (e -> runIO (f e) kp kf ke))

-- (|) : forall a. IO a -> IO a -> IO a
tryIO m n = IO (kp (kf : some r. K r) ke -> runIO m kp kf (_ -> runIO n kp kf ke))

ioEmpty = IO (_ (kf : some r. K r) ke -> kf (ke . Exception . EmptyException) ke mkEmpty)

ioAlt = Alt ioEmpty tryIO ioAp

-- provisional
fixIO : forall a. (a -> IO a) -> IO a
fixIO f = let x = unsafePerformIO (f x) in IO (kp _ _ -> kp $! x)

-- runFFI : forall a. FFI a -> IO a
runFFI ffi = IO (kp (kf : some r. K r) ke -> kf kp ke ffi)

foreign
  data "com.clarifi.reporting.ermine.session.EmptyException" EmptyException
  subtype EmptyException : EmptyException -> Exception

private foreign
  data "java.io.PrintWriter" PrintWriter
  data "java.io.InputStream" InputStream
  data "java.io.Console"     Console
  constructor mkEmpty : FFI EmptyException
  function "java.lang.System" "console" mkConsole : Console
  value "java.lang.System" "out" sysout : PrintWriter
  value "java.lang.System" "in" sysin : InputStream
  method "writer" writer# : Console -> PrintWriter
  method "readLine" readLn# : Console -> IO String
  method "println" printLn# : PrintWriter -> String -> IO ()

-- printLn : String -> IO ()
printLn s = printLn# (writer# mkConsole) s

trace : String -> a -> a
trace s a = unsafePerformIO (printLn s >>= (_ -> ioReturn a))

traceBy : (a -> b) -> a -> a
traceBy logger a = trace (toString (logger a)) a

traceShow : a -> a
traceShow a = trace (toString a) a

traceShowS: String -> a -> a
traceShowS s a = trace (s ++ (toString a)) a

-- readLn : IO String
readLn = readLn# mkConsole
