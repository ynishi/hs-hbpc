{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Domain.Usecase
  ( Req(..)
  , Res(..)
  , Store(..)
  , DataS(..)
  , cbpBlueprintTitle
  , cbpBlueprintName
  , cbpDevices
  , createBlueprint
  ) where

import qualified Algebra.Graph          as AG
import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Control.Lens           as CL
import           Control.Monad.IO.Class
import           Control.Monad.STM
import qualified Domain.Blueprint       as B
import qualified Domain.Device          as D

-- for controller
data Req
  = Req
  | CreateBlueprintReq { _cbpBlueprintTitle :: String
                       , _cbpBlueprintName  :: String
                       , _cbpDevices        :: [D.Name] }
  | CreateDeviceReq { _cdDeviceTitle :: String
                    , _cdDeviceName  :: String }

data Res
  = Res
  | ResMsg String
  | CreateBlueprintRes (Either String String)
  | CreateDeviceRes (Either String String)

CL.makeLenses ''Req

createBlueprint :: (Store a) => a -> Req -> IO Res
createBlueprint st (CreateBlueprintReq title name devices) = do
  let blueprint = B.defaultBlueprint
  isExists <- mapM (isExistsDevice st . fromDeviceName) devices
  if and isExists
    then do
      fetchedDevices <- fetchDeviceIn st devices
      let fetchedDevices' = map fromDeviceDataS fetchedDevices
      let blueprint' = foldr B.addDevice blueprint fetchedDevices'
      saveBlueprint st $ fromBlueprint blueprint'
      return . CreateBlueprintRes . Right $ "OK"
    else return . CreateBlueprintRes . Left $ "NG"

registDevice :: (Store a) => a -> Req -> IO Res
registDevice st (CreateDeviceReq title name) = do
  isExists <- isExistsDevice st DeviceDataS
  if isExists
    then return $ CreateDeviceRes . Right $ "Exists"
    else do
      storeDevice st DeviceDataS
      return $ CreateDeviceRes . Left $ "OK"

-- for Data store
data DataS
  = BlueprintDataS
  | DeviceDataS

fromDeviceDataS :: DataS -> D.Device
fromDeviceDataS ds = D.defaultDevice

fromBlueprint :: B.Blueprint -> DataS
fromBlueprint bp = BlueprintDataS

fromDeviceName :: D.Name -> DataS
fromDeviceName d = DeviceDataS

class Store a where
  store :: a -> DataS -> IO ()
  store _ _ = return ()
  storeDevice :: a -> DataS -> IO ()
  storeDevice _ _ = return ()
  saveBlueprint :: a -> DataS -> IO ()
  saveBlueprint _ _ = return ()
  fetchAll :: a -> IO [DataS]
  fetchAll _ = return []
  fetchDeviceBy :: a -> D.Name -> IO DataS
  fetchDeviceBy _ _ = return DeviceDataS
  fetchDeviceIn :: a -> [D.Name] -> IO [DataS]
  fetchDeviceIn _ _ = return []
  isExistsDevice :: a -> DataS -> IO Bool
