{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS -Wall -Werror -fno-warn-unused-binds #-}

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

import qualified Control.Lens     as CL
import qualified Domain.Blueprint as B
import qualified Domain.Device    as D

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
createBlueprint _ Req = return Res
createBlueprint _ (CreateDeviceReq _ _) = return Res
createBlueprint st (CreateBlueprintReq _ _ devices) = do
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
registDevice _ Req = return Res
registDevice _ CreateBlueprintReq {} = return Res
registDevice st (CreateDeviceReq _ _) = do
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
  deriving (Eq)

fromDeviceDataS :: DataS -> D.Device
fromDeviceDataS _ = D.defaultDevice

fromBlueprint :: B.Blueprint -> DataS
fromBlueprint _ = BlueprintDataS

fromDeviceName :: D.Name -> DataS
fromDeviceName _ = DeviceDataS

class Store a where
  store :: a -> DataS -> IO ()
  storeDevice :: a -> DataS -> IO ()
  saveBlueprint :: a -> DataS -> IO ()
  fetchAll :: a -> IO [DataS]
  fetchDeviceBy :: a -> D.Name -> IO DataS
  fetchDeviceIn :: a -> [D.Name] -> IO [DataS]
  isExistsDevice :: a -> DataS -> IO Bool
