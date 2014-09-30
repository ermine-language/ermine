module Native.Error where

export Native.Exception

foreign
  data "java.lang.Error" Error
  subtype Error : Error -> Exception
  constructor err : String -> Error
  constructor errWithCause : String -> Throwable -> Error
