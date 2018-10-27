{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module DataAdapter.MySQL
  ( MysqlStore(..)
  , DataAdapter.MySQL.defaultConnectInfo
  ) where

import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Database.Persist
import           Database.Persist.MySQL
import           Database.Persist.TH

import qualified Domain.Usecase               as U

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Blueprint
    name String
Device
    name String
|]

newtype MysqlStore =
  MysqlStore ConnectInfo

instance U.Store MysqlStore where
  store (MysqlStore ci) ds = return ()
  fetchAll (MysqlStore ci) = do
    rows <- runDB ci getAll
    return . replicate (length rows) $ U.DefaultBlueprintDataS
    where
      getAll :: MonadIO m => ReaderT SqlBackend m [Entity Blueprint]
      getAll = rawSql "select ?? from Blueprint" []
  isExistsDevice (MysqlStore ci) ds = return True
  storeDevice _ _ = return ()
  storeBlueprint _ _ = return ()
  fetchDeviceIn _ _ = return []
  fetchDeviceByName _ _ = return []

defaultConnectInfo :: ConnectInfo
defaultConnectInfo =
  ConnectInfo
    { connectHost = "localhost"
    , connectPort = 3306
    , connectUser = "root"
    , connectPassword = ""
    , connectDatabase = "persist_test"
    , connectOptions = []
    , connectPath = ""
    , connectSSL = Nothing
    }

runDB :: ConnectInfo -> SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runDB info = runNoLoggingT . runResourceT . withMySQLConn info . runSqlConn
