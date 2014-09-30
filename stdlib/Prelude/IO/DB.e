module IO.DB where

import Scanners as S
import Runners as R
import DB
import Native.Relation

sqlServer conn = (sqlServer_R conn, sqlServer_S)
sqlServer2005 conn = (sqlServer_R conn , sqlServer2005_S)
mysql conn = (mySql_R conn , mySql_S)
mySqlInnoDB conn = (mySql_R conn , mySqlInnoDB_S)
postgres conn = (postgres_R conn , postgres_S)
sqlite conn = (sqlite_R conn , sqlite_S)

getRecords (runner, scanner) rel = run_R runner (runRelation scanner rel)
