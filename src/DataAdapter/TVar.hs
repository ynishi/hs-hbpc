{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS -Wall -Werror -fno-warn-unused-binds #-}

module DataAdapter.TVar
  ( TVarStore(..)
  , defaultTVarStore
  ) where

import qualified Control.Concurrent.STM as CCSTM
import           Control.Monad.IO.Class
import           Control.Monad.STM
import qualified Domain.Usecase         as U

newtype TVarStore =
  TVarStore (CCSTM.TVar [U.DataS])

instance U.Store TVarStore where
  store (TVarStore db) ds = do
    liftIO . atomically . CCSTM.modifyTVar db $ (\ts -> ds : ts)
    return ()
  fetchAll (TVarStore db) = CCSTM.readTVarIO db
  isExistsDevice (TVarStore db) ds = do
    dataSs <- CCSTM.readTVarIO db
    let isExist = ds `elem` dataSs
    return isExist
  storeDevice _ _ = return ()
  saveBlueprint _ _ = return ()
  fetchDeviceIn _ _ = return []
  fetchDeviceBy _ _ = return U.DeviceDataS

defaultTVarStore :: IO TVarStore
defaultTVarStore = do
  tVar <- atomically $ CCSTM.newTVar []
  return $ TVarStore tVar
