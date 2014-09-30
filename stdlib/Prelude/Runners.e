module Runners where

import DB
import Native.Function

foreign

  data "com.clarifi.reporting.Run" Runner (f : * -> *)
  method "run" run : forall f a . Runner f -> f a -> IO a

  function "com.clarifi.reporting.backends.Runners" "MySQL" mySql: String -> Runner DB
  function "com.clarifi.reporting.backends.Runners" "Postgres" postgres: String -> Runner DB
  function "com.clarifi.reporting.backends.Runners" "Vertica" vertica: String -> Runner DB
  function "com.clarifi.reporting.backends.Runners" "SQLite" sqlite: String -> Runner DB
  function "com.clarifi.reporting.backends.Runners" "MicrosoftSQLServer" sqlServer: String -> Runner DB
  function "com.clarifi.reporting.backends.DB" "Run" buildRunner#: String -> Function1 String (Runner DB)

buildRunner driver connection = funcall1# (buildRunner# driver) connection