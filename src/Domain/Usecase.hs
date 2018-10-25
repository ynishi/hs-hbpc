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
  , fromBlueprint
  , fromDataS
  , DataS(..)
  , CreateNewBlueprintReq(..)
  , CreateNewBlueprintRes(..)
  , AddBlueprintRes(..)
  , AddBlueprintReq(..)
  , AddDeviceToBlueprintRes(..)
  , AddDeviceToBlueprintReq(..)
  , LinkDeviceRes(..)
  , LinkDeviceReq(..)
  , LoadBlueprintRes(..)
  , LoadBlueprintReq(..)
  , LoadBlueprintResData(..)
  , LoadDeviceRes(..)
  , LoadDeviceReq(..)
  , LoadDeviceResData(..)
  , LoadDeviceByBlueprintRes(..)
  , LoadDeviceByBlueprintReq(..)
  , LoadLinkRes(..)
  , LoadLinkReq(..)
  , LoadLinkResData(..)
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
  , linkDevice
  , linkDeviceMaybe
  , loadLink
  , loadBlueprint
  , loadBlueprintMaybe
  , saveBlueprint
  , loadDevice
  , registDevice
  , addDeviceToBlueprint
  ) where

import qualified Algebra.Graph             as AG
import qualified Control.Exception.Safe    as CES
import qualified Control.Lens              as CL
import qualified Control.Monad.IO.Class    as CMIC
import qualified Control.Monad.Trans.Maybe as CMTM
import qualified Data.List                 as List
import qualified Data.Map.Strict           as Map
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

data LinkDeviceReq = LinkDeviceReq
  { _lidrBlueprint :: String
  , _lidrIface     :: String
  , _lidrDeviceX   :: String
  , _lidrDeviceY   :: String
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

data LoadLinkReq = LoadLinkReq
  { _llrName  :: String
  , _llrIFace :: String
  }

data RegistDeviceReq = RegistDeviceReq
  { _rdrName   :: String
  , _rdrDesc   :: String
  , _rdrIfaces :: [String]
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

newtype LinkDeviceRes =
  LinkDeviceRes (Either String String)
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

newtype LoadLinkRes =
  LoadLinkRes (Either String [LoadLinkResData])
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
  | LoadBlueprintResData { _lbrdName    :: String
                         , _lbrdTitle   :: String
                         , _lbrdDesc    :: String
                         , _lbrdDevices :: [LoadDeviceResData] }
  deriving (Show, Eq)

data LoadDeviceResData
  = LoadDeviceResDataEmpty
  | LoadDeviceResData { _ldrdName   :: String
                      , _ldrdDesc   :: String
                      , _ldrdIfaces :: [String] }
  deriving (Show, Eq)

data LoadLinkResData
  = LoadLinkResDataEmpty
  | LoadLinkResData { _llrdName   :: String
                    , _llrdIface  :: String
                    , _llrdDevice :: [String] }
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

linkDeviceMaybe :: Store a => a -> LinkDeviceReq -> IO (Maybe LinkDeviceRes)
linkDeviceMaybe db req = do
  CMTM.runMaybeT $ do
    fetchedBp <- CMTM.MaybeT $ fetchBlueprintByName db bpName
    let bp = head . map fromBlueprintDataS $ fetchedBp
    fetchedD1 <- CMTM.MaybeT $ fetchDeviceByNameMaybe db d1Name
    let d1 = head . map fromDeviceDataS $ fetchedD1
    fetchedD2 <- CMTM.MaybeT $ fetchDeviceByNameMaybe db d2Name
    let d2 = head . map fromDeviceDataS $ fetchedD2
    let linked = B.link d1 d2 iface bp
    CMIC.liftIO . storeLink db . fromBlueprintToLinkDataS linked $ iface
    return . LinkDeviceRes . Right $ (B._bpName bp) ++ ":" ++ iface ++ ":" ++
      d1Name ++
      ":" ++
      d2Name ++
      "linked:" ++
      (show linked) ++
      "::bp:" ++
      (show bp)
  where
    bpName = _lidrBlueprint req
    d1Name = _lidrDeviceX req
    d2Name = _lidrDeviceY req
    iface = _lidrIface req

linkDevice :: (Store a) => a -> LinkDeviceReq -> IO LinkDeviceRes
linkDevice db req = do
  fetchedBp <- fetchBlueprintByName db bpName
  fetchedDev1 <- fetchDeviceByName db d1Name
  fetchedDev2 <- fetchDeviceByName db d2Name
  case fetchedBp of
    Nothing -> return . LinkDeviceRes . Left $ "not found:" ++ bpName
    Just bpl -> do
      case fetchedDev1 of
        [] -> return . LinkDeviceRes . Left $ "not found:" ++ d1Name
        (fdl1:_) -> do
          case fetchedDev2 of
            [] -> return . LinkDeviceRes . Left $ "not found:" ++ d2Name
            (fdl2:_) -> do
              let fd1 = fromDeviceDataS fdl1
              let fd2 = fromDeviceDataS fdl2
              let bp = fromBlueprintDataS . head $ bpl
              if List.elem iface (fd1 CL.^. D.deviceIfaces)
                then if List.elem iface (fd2 CL.^. D.deviceIfaces)
                       then do
                         let linked = B.link fd1 fd2 iface bp
                         storeLink db . fromBlueprintToLinkDataS linked $ iface
                         return . LinkDeviceRes . Right $ (B._bpName bp) ++ ":" ++
                           iface ++
                           ":" ++
                           d1Name ++
                           ":" ++
                           d2Name
                       else return . LinkDeviceRes . Left $ "not had:" ++ d2Name ++
                            iface
                else return . LinkDeviceRes . Left $ "not had:" ++ d1Name ++
                     iface
  where
    bpName = _lidrBlueprint req
    d1Name = _lidrDeviceX req
    d2Name = _lidrDeviceY req
    iface = _lidrIface req

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

loadLink :: (Store a) => a -> LoadLinkReq -> IO LoadLinkRes
loadLink db req = do
  fetched <- fetchLinkByName db name
  case fetched of
    []    -> return . LoadLinkRes $ Left $ "load:" ++ name
    links -> return . LoadLinkRes $ Right . map fromDataSToLLR $ links
  where
    name = _llrName req
    iface = _llrIFace req
    fiface x = (_llrdIface x) == iface

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
registDevice st (RegistDeviceReq name desc ifaces) = do
  result <- fetchDeviceByName st name
  case result of
    [] -> do
      storeDevice st $ DeviceDataS name desc ifaces
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
          let iface = head . D._deviceIfaces $ deviceInstance
          let linkDataS = fromBlueprintToLinkDataS blueprint' iface
          storeLink st linkDataS
          resultUpdate <- CES.try $ updateBlueprint st blueprintDataS'
          case resultUpdate of
            Left (_ :: USException) ->
              return . AddDeviceToBlueprintRes . Left $ "update err:bp:" ++
              blueprintName
            Right _ ->
              return . AddDeviceToBlueprintRes . Right $ deviceName ++ ":" ++
              blueprintName ++
              ":linkdataS" ++
              (show linkDataS) ++
              ":iface:" ++
              iface

-- for Data store
data DataS
  = DefaultBlueprintDataS
  | DefaultDeviceDataS
  | BlueprintDataS { _bpdsName    :: String
                   , _bpdsTitle   :: String
                   , _bpdsDesc    :: String
                   , _bpdsDevices :: [DataS] }
  | DeviceDataS { _ddsName   :: String
                , _ddsDesc   :: String
                , _ddsIfaces :: [String] }
  | LinkDataS { _ldsName    :: String
              , _ldsIface   :: String
              , _ldsDevices :: [String] }
  deriving (Show, Eq)

-- CL.makeLenses ''DataS
defaultDeviceDataS :: DataS
defaultDeviceDataS = DeviceDataS {_ddsName = "", _ddsDesc = "", _ddsIfaces = []}

fromDeviceDataS :: DataS -> D.Device
fromDeviceDataS dataS =
  D.defaultDevice
    { D._deviceName = _ddsName dataS
    , D._deviceDesc = _ddsDesc dataS
    , D._deviceIfaces = _ddsIfaces dataS
    }

fromDevice :: D.Device -> DataS
fromDevice device =
  DeviceDataS
    { _ddsName = device CL.^. D.deviceName
    , _ddsDesc = device CL.^. D.deviceDesc
    , _ddsIfaces = device CL.^. D.deviceIfaces
    }

fromBlueprint :: B.Blueprint -> DataS
fromBlueprint blueprint =
  BlueprintDataS
    { _bpdsName = blueprint CL.^. B.bpName
    , _bpdsTitle = blueprint CL.^. B.bpTitle
    , _bpdsDesc = blueprint CL.^. B.bpDesc
    , _bpdsDevices = map fromDevice $ (blueprint CL.^. B.bpDevices)
    }

-- TODO Devices
fromBlueprintToLinkDataS :: B.Blueprint -> String -> DataS
fromBlueprintToLinkDataS blueprint iface =
  LinkDataS
    { _ldsName = blueprint CL.^. B.bpName
    , _ldsIface = iface
    , _ldsDevices =
        map show . AG.edgeList . Map.findWithDefault AG.empty iface $ blueprint CL.^.
        B.bpGraphs
    }

fromBlueprintDataS :: DataS -> B.Blueprint
fromBlueprintDataS dataS =
  B.defaultBlueprint
    { B._bpName = _bpdsName dataS
    , B._bpTitle = _bpdsTitle dataS
    , B._bpDesc = _bpdsDesc dataS
    , B._bpDevices = map fromDeviceDataS $ (_bpdsDevices dataS)
    }

fromDeviceName :: D.Name -> DataS
fromDeviceName name = defaultDeviceDataS {_ddsName = name}

fromDataS :: DataS -> LoadBlueprintResData
fromDataS (BlueprintDataS name title desc devices) =
  LoadBlueprintResData name title desc (map fromDataSToLDRD devices)
fromDataS _ = LoadBlueprintResDataEmpty

fromDataSToLDRD :: DataS -> LoadDeviceResData
fromDataSToLDRD (DeviceDataS name desc ifaces) =
  LoadDeviceResData name desc ifaces
fromDataSToLDRD _ = LoadDeviceResDataEmpty

fromDataSToLLR :: DataS -> LoadLinkResData
fromDataSToLLR (LinkDataS name iface devices) =
  LoadLinkResData name iface devices
fromDataSToLLR _ = LoadLinkResDataEmpty

class Store a where
  store :: a -> DataS -> IO ()
  storeDevice :: a -> DataS -> IO ()
  storeBlueprint :: a -> DataS -> IO ()
  storeLink :: a -> DataS -> IO ()
  insertBlueprint :: a -> DataS -> IO ()
  updateBlueprint :: a -> DataS -> IO ()
  fetchAll :: a -> IO [DataS]
  fetchDeviceByName :: a -> D.Name -> IO [DataS]
  fetchDeviceByNameMaybe :: a -> D.Name -> IO (Maybe [DataS])
  fetchDeviceIn :: a -> [D.Name] -> IO [DataS]
  fetchBlueprintBy :: a -> DataS -> IO (Maybe [DataS])
  fetchBlueprintByName :: a -> String -> IO (Maybe [DataS])
  fetchBlueprintByNameThrow :: a -> String -> IO [DataS]
  fetchLinkByName :: a -> String -> IO [DataS]
  isExistsDevice :: a -> DataS -> IO Bool
  isExistsBlueprint :: a -> String -> IO Bool
