module Scanners where

import DB
import Native.Function
import Vector
import Native.Record
import Relation

foreign
  data "com.clarifi.reporting.relational.Scanner" Scanner (f: * -> *)

  private
    data "com.clarifi.reporting.backends.Scanners$" ScannersModule
    value "com.clarifi.reporting.backends.Scanners$" "MODULE$" scannersModule : ScannersModule

  method "StateScanner" stateScanner# : ScannersModule -> Scanner f
  method "MySQLInnoDB" mySqlInnoDB# : ScannersModule -> Scanner f
  method "MySQL" mySql#: ScannersModule -> Scanner f
  method "Postgres" postgres#: ScannersModule -> Scanner f
  method "Vertica" vertica#: ScannersModule -> Scanner f
  method "SQLite" sqlite#: ScannersModule -> Scanner f
  method "MicrosoftSQLServer" sqlServer#: ScannersModule -> Scanner f
  method "MicrosoftSQLServer2005" sqlServer2005#: ScannersModule -> Scanner f

stateScanner = stateScanner# scannersModule
mySqlInnoDB = mySqlInnoDB# scannersModule
mySql = mySql# scannersModule
postgres = postgres# scannersModule
vertica = vertica# scannersModule
sqlite = sqlite# scannersModule
sqlServer = sqlServer# scannersModule
sqlServer2005 = sqlServer2005# scannersModule

