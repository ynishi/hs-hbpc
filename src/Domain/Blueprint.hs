{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Domain.Blueprint
  ( Blueprint(..)
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

data Blueprint
  = Empty
  | Blueprint { _bpName    :: Name
              , _bpTitle   :: String
              , _bpDesc    :: String
              , _bpDevices :: [D.Device]
              , _bpGraphs  :: Map.Map String (AG.Graph D.Device) }
  deriving (Eq, Show)

type Name = String

CL.makeLenses ''Blueprint

empty = Empty

defaultBlueprint =
  Blueprint
    { _bpName = ""
    , _bpTitle = ""
    , _bpDesc = ""
    , _bpDevices = []
    , _bpGraphs = Map.empty
    }

addDevice :: D.Device -> Blueprint -> Blueprint
addDevice device = (bpDevices CL.%~ (device :)) . (bpGraphs CL.%~ updateGraphs)
  where
    ps = device CL.^. D.deviceIfaces
    ver = AG.vertex device
    updateGraphs graphs =
      foldl
        (\graphs' protocol -> Map.insertWith AG.overlay protocol ver graphs')
        graphs
        ps

link :: D.Device -> D.Device -> String -> Blueprint -> Blueprint
link device1 device2 iface = bpGraphs CL.%~ updateGraphs
  where
    updateGraphs graphs =
      Map.update
        (\graph -> do
           let e = AG.edge device1 device2
           Just $ AG.overlay e graph)
        iface
        graphs
