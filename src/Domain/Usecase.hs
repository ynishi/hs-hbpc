{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS -Wall -Werror -fno-warn-unused-binds #-}

module Domain.Usecase
  ( Req(..)
  , Res(..)
  , Store(..)
  , DataS(..)
  , CreateNewBlueprintReq(..)
  , CreateNewBlueprintRes(..)
  , AddBlueprintRes(..)
  , AddBlueprintReq(..)
  , AddDeviceToBlueprintRes(..)
  , AddDeviceToBlueprintReq(..)
  , LoadBlueprintRes(..)
  , LoadBlueprintReq(..)
  , LoadBlueprintResData(..)
  , LoadDeviceRes(..)
  , LoadDeviceReq(..)
  , LoadDeviceResData(..)
  , LoadDeviceByBlueprintRes(..)
  , LoadDeviceByBlueprintReq(..)
  , RegistDeviceRes(..)
  , RegistDeviceReq(..)
  , SaveBlueprintRes(..)
  , SaveBlueprintReq(..)
  , USException(..)
  , cbpBlueprintTitle
  , cbpBlueprintName
  , cbpDevices
  , createBlueprint
  , createNewBlueprint
  , addBlueprint
  , loadBlueprint
  , loadBlueprintMaybe
  , saveBlueprint
  , loadDevice
  , loadDeviceByBlueprint
  , registDevice
  , addDeviceToBlueprint
  ) where

import qualified Control.Exception.Safe    as CES
import qualified Control.Lens              as CL
import qualified Control.Monad.IO.Class    as CMIC
import qualified Control.Monad.Trans.Maybe as CMTM
import qualified Domain.Blueprint          as B
import qualified Domain.Device             as D

data USException
  = InsertException String
  | NotFoundException String
  deriving (Show)

instance CES.Exception USException

-- for controller
data Req
  = Req
  | CreateBlueprintReq { _cbpBlueprintTitle :: String
                       , _cbpBlueprintName  :: String
                       , _cbpDevices        :: [D.Name] }
  | CreateDeviceReq { _cdDeviceTitle :: String
                    , _cdDeviceName  :: String }

data CreateNewBlueprintReq =
  CreateNewBlueprintReq

data AddBlueprintReq = AddBlueprintReq
  { _adrName  :: String
  , _adrTitle :: String
  , _adrDesc  :: String
  }

newtype LoadBlueprintReq = LoadBlueprintReq
  { _lbrName :: String
  }

newtype LoadDeviceReq = LoadDeviceReq
  { _ldrName :: String
  }

newtype LoadDeviceByBlueprintReq = LoadDeviceByBlueprintReq
  { _ldbbbrName :: String
  }

data RegistDeviceReq = RegistDeviceReq
  { _rdrName  :: String
  , _rdrTitle :: String
  , _rdrDesc  :: String
  }

data SaveBlueprintReq = SaveBlueprintReq
  { _sbrName  :: String
  , _sbrTitle :: String
  , _sbrDesc  :: String
  }

data AddDeviceToBlueprintReq = AddDeviceToBlueprintReq
  { _adtbrDeviceName    :: String
  , _adtbrBlueprintName :: String
  }

newtype CreateNewBlueprintRes = CreateNewBlueprintRes
  { _cnbrName :: String
  } deriving (Show, Eq)

newtype AddBlueprintRes =
  AddBlueprintRes (Either String String)
  deriving (Show, Eq)

newtype LoadBlueprintRes =
  LoadBlueprintRes (Either String LoadBlueprintResData)
  deriving (Show, Eq)

newtype LoadDeviceRes =
  LoadDeviceRes (Either String LoadDeviceResData)
  deriving (Show, Eq)

newtype LoadDeviceByBlueprintRes =
  LoadDeviceByBlueprintRes (Either String LoadDeviceResData)
  deriving (Show, Eq)

newtype RegistDeviceRes =
  RegistDeviceRes (Either String String)
  deriving (Show, Eq)

newtype SaveBlueprintRes =
  SaveBlueprintRes (Either String String)
  deriving (Show, Eq)

newtype AddDeviceToBlueprintRes =
  AddDeviceToBlueprintRes (Either String String)
  deriving (Show, Eq)

data LoadBlueprintResData
  = LoadBlueprintResDataEmpty
  | LoadBlueprintResData { _lbrdName  :: String
                         , _lbrdTitle :: String
                         , _lbrdDesc  :: String }
  deriving (Show, Eq)

data LoadDeviceResData
  = LoadDeviceResDataEmpty
  | LoadDeviceResData { _ldrdName  :: String
                      , _ldrdTitle :: String
                      , _ldrdDesc  :: String }
  deriving (Show, Eq)

data Res
  = Res
  | ResMsg String
  | CreateBlueprintRes (Either String String)
  | CreateDeviceRes (Either String String)

CL.makeLenses ''Req

CL.makeLenses ''AddBlueprintReq

createNewBlueprint ::
     (Store a) => a -> CreateNewBlueprintReq -> IO CreateNewBlueprintRes
createNewBlueprint st _ = do
  takenM <- takeWhileNextM (isExistsBlueprint st) ids
  let name = last takenM
  return $ CreateNewBlueprintRes name
  where
    ids :: [String]
    ids = map (\i -> "untitled-" ++ show i) [(1 :: Integer) ..]
    takeWhileNextM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
    takeWhileNextM _ [] = return []
    takeWhileNextM p (x:xs) = do
      q <- p x
      if q
        then (:) x <$> takeWhileNextM p xs
        else return [x]

addBlueprint :: (Store a) => a -> AddBlueprintReq -> IO AddBlueprintRes
addBlueprint db req = do
  fetched <- fetchBlueprintBy db blueprintDataS
  case fetched of
    Nothing -> do
      result <- CES.try $ insertBlueprint db blueprintDataS
      case result of
        Left (_ :: USException) ->
          return . AddBlueprintRes . Left $ "insert:" ++ name
        Right _ -> return . AddBlueprintRes . Right $ "OK"
    _ -> return . AddBlueprintRes . Left $ "insert:" ++ name
  where
    name = _adrName req
    title = _adrTitle req
    desc = _adrDesc req
    blueprint =
      (B.bpName CL..~ name) . (B.bpTitle CL..~ title) . (B.bpDesc CL..~ desc) $
      B.defaultBlueprint
    blueprintDataS = fromBlueprint blueprint

loadBlueprintMaybe ::
     (Store a) => a -> LoadBlueprintReq -> IO (Maybe LoadBlueprintResData)
loadBlueprintMaybe db req =
  CMTM.runMaybeT $ do
    fetched <- CMTM.MaybeT $ fetchBlueprintByName db name
    let fetchedOne = head . map fromDataS $ fetched
    CMIC.liftIO . print $ "fetched:" ++ show fetchedOne
    return fetchedOne
  where
    name = _lbrName req

loadBlueprint :: (Store a) => a -> LoadBlueprintReq -> IO LoadBlueprintRes
loadBlueprint db req = do
  fetched <- fetchBlueprintByName db name
  case fetched of
    Nothing -> return . LoadBlueprintRes . Left $ "load:" ++ name
    Just bps -> return . LoadBlueprintRes . Right . head . map fromDataS $ bps
  where
    name = _lbrName req

saveBlueprint :: (Store a) => a -> SaveBlueprintReq -> IO SaveBlueprintRes
saveBlueprint db req = do
  fetched <- fetchBlueprintByName db name
  case fetched of
    Nothing -> return . SaveBlueprintRes . Left $ "not found:save:" ++ name
    Just _ -> do
      result <- CES.try $ updateBlueprint db blueprintDataS
      case result of
        Left (_ :: USException) ->
          return . SaveBlueprintRes . Left $ "update err:save:" ++ name
        Right _ -> return . SaveBlueprintRes . Right $ name
  where
    name = _sbrName req
    title = _sbrTitle req
    desc = _sbrDesc req
    blueprint =
      B.defaultBlueprint CL.& (B.bpName CL..~ name) . (B.bpTitle CL..~ title) .
      (B.bpDesc CL..~ desc)
    blueprintDataS = fromBlueprint blueprint

loadDevice :: (Store a) => a -> LoadDeviceReq -> IO LoadDeviceRes
loadDevice db req = do
  fetched <- fetchDeviceByName db name
  case fetched of
    []  -> return . LoadDeviceRes . Left $ "load:" ++ name
    bps -> return . LoadDeviceRes . Right . head . map fromDataSToLDRD $ bps
  where
    name = _ldrName req

loadDeviceByBlueprint ::
     (Store a) => a -> LoadDeviceByBlueprintReq -> IO LoadDeviceByBlueprintRes
loadDeviceByBlueprint db req = do
  fetched <- fetchDeviceByBlueprintName db name
  case fetched of
    []  -> return . LoadDeviceRes . Left $ "load:" ++ name
    bps -> return . LoadDeviceRes . Right . head . map fromDataSToLDRD $ bps
  where
    name = _ldbbbrName req

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
      storeBlueprint st $ fromBlueprint blueprint'
      return . CreateBlueprintRes . Right $ "OK"
    else return . CreateBlueprintRes . Left $ "NG"

registDevice :: (Store a) => a -> RegistDeviceReq -> IO RegistDeviceRes
registDevice st (RegistDeviceReq name title desc) = do
  result <- fetchDeviceByName st name
  case result of
    [] -> do
      storeDevice st $ DeviceDataS name title desc
      return . RegistDeviceRes . Right $ "OK"
    _ -> return . RegistDeviceRes . Left $ "Exists:" ++ name

addDeviceToBlueprint ::
     (Store a) => a -> AddDeviceToBlueprintReq -> IO AddDeviceToBlueprintRes
addDeviceToBlueprint st (AddDeviceToBlueprintReq deviceName blueprintName) = do
  resultDevice <- fetchDeviceByName st deviceName
  resultBlueprint <- fetchBlueprintByName st blueprintName
  case resultDevice of
    [] ->
      return . AddDeviceToBlueprintRes . Left $ "not found device:" ++
      deviceName
    (deviceRes:_) ->
      case resultBlueprint of
        Nothing ->
          return . AddDeviceToBlueprintRes . Left $ "not found bp:" ++
          blueprintName
        Just [] ->
          return . AddDeviceToBlueprintRes . Left $ "not found bp:" ++
          blueprintName
        Just (blueprintRes:_) -> do
          let deviceInstance = fromDeviceDataS deviceRes
          let blueprintInstance = fromBlueprintDataS blueprintRes
          let blueprint' = B.addDevice deviceInstance blueprintInstance
          let blueprintDataS' = fromBlueprint blueprint'
          resultUpdate <- CES.try $ updateBlueprint st blueprintDataS'
          case resultUpdate of
            Left (_ :: USException) ->
              return . AddDeviceToBlueprintRes . Left $ "update err:bp:" ++
              blueprintName
            Right _ ->
              return . AddDeviceToBlueprintRes . Right $ deviceName ++ ":" ++
              blueprintName

-- for Data store
data DataS
  = DefaultBlueprintDataS
  | BlueprintDataS { _bpdsName  :: String
                   , _bpdsTitle :: String
                   , _bpdsDesc  :: String }
  | DeviceDataS { _ddsName  :: String
                , _ddsTitle :: String
                , _ddsDesc  :: String }
  deriving (Show, Eq)

-- CL.makeLenses ''DataS
defaultDeviceDataS :: DataS
defaultDeviceDataS = DeviceDataS {_ddsName = "", _ddsTitle = "", _ddsDesc = ""}

fromDeviceDataS :: DataS -> D.Device
fromDeviceDataS _ = D.defaultDevice

fromBlueprint :: B.Blueprint -> DataS
fromBlueprint blueprint =
  BlueprintDataS
    { _bpdsName = blueprint CL.^. B.bpName
    , _bpdsTitle = blueprint CL.^. B.bpTitle
    , _bpdsDesc = blueprint CL.^. B.bpDesc
    }

fromBlueprintDataS :: DataS -> B.Blueprint
fromBlueprintDataS dataS =
  B.defaultBlueprint
    { B._bpName = _bpdsName dataS
    , B._bpTitle = _bpdsTitle dataS
    , B._bpDesc = _bpdsDesc dataS
    }

fromDeviceName :: D.Name -> DataS
fromDeviceName name = defaultDeviceDataS {_ddsName = name}

fromDataS :: DataS -> LoadBlueprintResData
fromDataS (BlueprintDataS name title desc) =
  LoadBlueprintResData name title desc
fromDataS _ = LoadBlueprintResDataEmpty

fromDataSToLDRD :: DataS -> LoadDeviceResData
fromDataSToLDRD (DeviceDataS name title desc) =
  LoadDeviceResData name title desc
fromDataSToLDRD _ = LoadDeviceResDataEmpty

class Store a where
  store :: a -> DataS -> IO ()
  storeDevice :: a -> DataS -> IO ()
  storeBlueprint :: a -> DataS -> IO ()
  insertBlueprint :: a -> DataS -> IO ()
  updateBlueprint :: a -> DataS -> IO ()
  fetchAll :: a -> IO [DataS]
  fetchDeviceByName :: a -> D.Name -> IO [DataS]
  fetchDeviceByBlueprintName :: a -> D.Name -> IO [DataS]
  fetchDeviceIn :: a -> [D.Name] -> IO [DataS]
  fetchBlueprintBy :: a -> DataS -> IO (Maybe [DataS])
  fetchBlueprintByName :: a -> String -> IO (Maybe [DataS])
  fetchBlueprintByNameThrow :: a -> String -> IO [DataS]
  isExistsDevice :: a -> DataS -> IO Bool
  isExistsBlueprint :: a -> String -> IO Bool
