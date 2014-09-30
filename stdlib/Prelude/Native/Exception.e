module Native.Exception where

export Native.Throwable

foreign
  data "java.lang.Exception" Exception
  subtype Exception : Exception -> Throwable
  constructor exception : String -> Exception
  constructor exceptionWithCause : String -> Throwable -> Exception
