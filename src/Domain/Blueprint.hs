{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Domain.Blueprint
  ( Blueprint(..)
  , Graphs
  , Graph
  , Devices
  , addDevice
  , bpDesc
  , bpDevices
  , bpGraphs
  , bpName
  , bpTitle
  , defaultBlueprint
  , empty
  , link
  ) where

import qualified Algebra.Graph   as AG
import qualified Control.Lens    as CL
import qualified Data.List       as List
import qualified Data.Map.Strict as Map
import qualified Domain.Device   as D
import qualified Domain.Types    as T

data Blueprint
  = Empty
  | Blueprint { _bpName    :: T.Name
              , _bpTitle   :: T.Title
              , _bpDesc    :: T.Desc
              , _bpDevices :: Devices
              , _bpGraphs  :: Graphs }
  deriving (Eq, Show)

type Graph = AG.Graph T.DeviceId

type Graphs = Map.Map T.Iface Graph

type Devices = Map.Map T.DeviceId D.Device

CL.makeLenses ''Blueprint

empty = Empty

defaultBlueprint =
  Blueprint
    { _bpName = ""
    , _bpTitle = ""
    , _bpDesc = ""
    , _bpDevices = Map.empty
    , _bpGraphs = Map.empty
    }

addDevice :: D.Device -> Blueprint -> Blueprint
addDevice device blueprint =
  (bpDevices CL.%~ updateDevice) . (bpGraphs CL.%~ updateGraphs) $ blueprint
  where
    ps = device CL.^. D.deviceIfaces
    deviceId = (+ 1) . maximum . Map.keys $ _bpDevices blueprint
    ver = AG.vertex deviceId
    updateGraphs graphs =
      foldl
        (\graphs' protocol -> Map.insertWith AG.overlay protocol ver graphs')
        graphs
        ps
    updateDevice = Map.insert deviceId device

type IsLinkable = Bool

linkable :: IsLinkable = True

notLinkable :: IsLinkable = False

link :: T.DeviceId -> T.DeviceId -> String -> Blueprint -> Blueprint
link deviceId1 deviceId2 iface blueprint =
  bpGraphs CL.%~ updateGraphs isLinkable $ blueprint
  where
    maybeHasProtocol :: T.Iface -> Maybe D.Device -> Bool
    maybeHasProtocol iface = maybe False (D.hasProtocol iface)
    hasProtocolById deviceId =
      maybeHasProtocol iface $ Map.lookup deviceId (_bpDevices blueprint)
    isLinkable :: IsLinkable = all hasProtocolById [deviceId1, deviceId2]
    updateGraphs :: IsLinkable -> Graphs -> Graphs
    updateGraphs linkable graphs =
      Map.update
        (\graph -> do
           let e = AG.edge deviceId1 deviceId2
           Just . AG.simplify . AG.overlay e $ graph)
        iface
        graphs
    updateGraphs notLinkable graphs = graphs
