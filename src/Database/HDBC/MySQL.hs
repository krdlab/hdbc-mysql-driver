{-# LANGUAGE OverloadedStrings #-}

module Database.HDBC.MySQL
    ( Connection
    , connect
    , test
    ) where

import Database.HDBC.Types (IConnection(..))
import Data.ByteString.Char8 (pack)
import qualified Database.MySQL.Base as MySQL

data Connection = Connection
    { unwrap   :: MySQL.Connection
    , connInfo :: MySQL.ConnectInfo
    }

connect :: MySQL.ConnectInfo -> IO Connection
connect info = do
    conn <- MySQL.connect info
    return Connection { unwrap = conn, connInfo = info }

instance IConnection Connection where
    disconnect (Connection c _) = MySQL.close c
    commit (Connection c _) = MySQL.commit c
    rollback (Connection c _) = MySQL.rollback c
    run (Connection c _) query _ = do    -- TODO: params
        MySQL.query c (pack query)
        toInteger <$> MySQL.affectedRows c
    prepare _ _ = error "TODO"
    clone (Connection _ info) = connect info
    hdbcDriverName _ = "mysql"
    hdbcClientVer = error "TODO"
    proxiedClientName _ = "mysql"
    proxiedClientVer = error "TODO"
    dbServerVer = error "TODO"
    dbTransactionSupport _ = True
    getTables = error "TODO"
    describeTable = error "TODO"

test :: MySQL.ConnectInfo -> IO ()
test info = do
    c <- connect info
    _ <- run c "SELECT 1 + 1" []
    r <- fetchRow c
    print r
    disconnect c
  where
    fetchRow (Connection c _) =
        MySQL.useResult c >>= MySQL.fetchRow
