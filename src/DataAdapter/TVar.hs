{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS -Wall -Werror -fno-warn-unused-binds #-}

module DataAdapter.TVar
  ( TVarStore(..)
  , defaultTVarStore
  ) where

import qualified Control.Concurrent.STM as CCSTM
import qualified Control.Exception.Safe as CES
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
  storeDevice = U.store
  storeBlueprint = U.store
  insertBlueprint (TVarStore db) ds = do
    dataSs <- CCSTM.readTVarIO db
    if ds `elem` dataSs
      then CES.throw $ U.InsertException "insertError"
      else liftIO . atomically . CCSTM.modifyTVar db $ (\ts -> ds : ts)
  updateBlueprint (TVarStore db) ds = do
    dataSs <- CCSTM.readTVarIO db
    if U._bpdsName ds `elem`
       map U._bpdsName (filter (\x -> U._bpdsName x /= "") dataSs)
      then liftIO . atomically . CCSTM.modifyTVar db $
           map
             (\case
                U.BlueprintDataS name title desc ->
                  if name == U._bpdsName ds
                    then U.BlueprintDataS
                           name
                           (U._bpdsTitle ds)
                           (U._bpdsDesc ds)
                    else U.BlueprintDataS name title desc
                U.DefaultBlueprintDataS -> U.DefaultBlueprintDataS
                U.DeviceDataS -> U.DeviceDataS)
      else CES.throw $ U.InsertException "updateError"
  fetchDeviceIn _ _ = return []
  fetchDeviceBy _ _ = return U.DeviceDataS
  fetchBlueprintBy (TVarStore db) ds = do
    dataSs <- CCSTM.readTVarIO db
    return . wrap $ filter (== ds) dataSs
    where
      wrap [] = Nothing
      wrap xs = Just xs
  fetchBlueprintByName (TVarStore db) name = do
    dataSs <- CCSTM.readTVarIO db
    return . wrap $ filter byName dataSs
    where
      byName :: U.DataS -> Bool
      byName U.DefaultBlueprintDataS  = False
      byName U.DeviceDataS            = False
      byName (U.BlueprintDataS s _ _) = s == name
      wrap [] = Nothing
      wrap xs = Just xs
  isExistsBlueprint (TVarStore db) name = do
    ds <- CCSTM.readTVarIO db
    let ds' = map (\(U.BlueprintDataS bdsName _ _) -> bdsName == name) ds
    return $ or ds'

defaultTVarStore :: IO TVarStore
defaultTVarStore = do
  tVar <- atomically $ CCSTM.newTVar []
  return $ TVarStore tVar
