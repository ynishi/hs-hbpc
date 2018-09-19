{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Domain.Blueprint
  ( Blueprint(..)
  , Components(..)
  , Component(..)
  , addDevice
  , addDeviceToBp
  , link
  , defaultBlueprint
  , empty
  , near
  , unlink
  , bpTitle
  , bpName
  , bpDesc
  , bpDevices
  , bpComponents
  , bpGraph
  , bpTCPIP
  , bpProtocols
  ) where

import qualified Algebra.Graph   as AG
import qualified Control.Lens    as CL
import qualified Data.List       as List
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import qualified Domain.Device   as DD

data Blueprint
  = Empty
  | Blueprint { _bpName       :: Name
              , _bpTitle      :: String
              , _bpDesc       :: String
              , _bpDevices    :: Set.Set DD.Device
              , _bpComponents :: [Components]
              , _bpGraph      :: AG.Graph DD.Device
              , _bpTCPIP      :: AG.Graph DD.Device
              , _bpProtocols  :: Map.Map String (AG.Graph DD.Device) }
  deriving (Eq, Show)

type Name = String

type Components = [Component]

data Component
  = End
  | Hub Name
        Components
  | HubDevice Name
              DD.Device
              Components
  deriving (Eq, Ord, Show)

CL.makeLenses ''Blueprint

empty = Empty

defaultBlueprint =
  Blueprint
    { _bpName = ""
    , _bpTitle = ""
    , _bpDesc = ""
    , _bpDevices = Set.empty
    , _bpComponents = []
    , _bpGraph = AG.empty
    , _bpTCPIP = AG.empty
    , _bpProtocols = Map.empty
    }

addDevice :: DD.Device -> Blueprint -> Blueprint
addDevice device =
  (bpDevices CL.%~ Set.insert device) .
  (bpGraph CL.%~ AG.overlay (AG.vertex device)) .
  (bpTCPIP CL.%~
   (\x ->
      if hasTCPIP device
        then AG.overlay (AG.vertex device) x
        else x))

hasTCPIP :: DD.Device -> Bool
hasTCPIP device = List.elem "tcpip" $ device CL.^. DD.deviceIfaces

addDeviceToBp :: DD.Device -> Blueprint -> Blueprint
addDeviceToBp device bp = bp {_bpGraph = added}
  where
    graph = AG.vertex device
    added = AG.overlay graph (_bpGraph bp)

name (Hub name _) = name
name End          = show End

near End        = []
near (Hub _ cs) = map (\c -> Hub (name c) []) . filter (/= End) $ cs

-- |
-- link
-- >>> link End End
-- (End,End)
link hub1 hub2 = (hub1', hub2')
  where
    append c1 c2 =
      case c1 of
        Hub name cs -> Hub name (c2 : cs)
        End         -> End
    hub1' = append hub1 hub2
    hub2' = append hub2 hub1

-- |
-- unlink
-- >>> unlink (Hub "hub1" [Hub "hub2" []]) (Hub "hub2" [Hub "hub1" []])
-- (Hub "hub1" [],Hub "hub2" [])
unlink hub1 hub2 = (hub1', hub2')
  where
    remove End _ = End
    remove (Hub name cs) (Hub name2 _) =
      Hub name $ filter (\(Hub n _) -> n /= name2) cs
    hub1' = remove hub1 hub2
    hub2' = remove hub2 hub1
