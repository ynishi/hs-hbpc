{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Domain.Blueprint
  ( Blueprint(..)
  , Components(..)
  , Component(..)
  , addDeviceToBp
  , link
  , defaultBlueprint
  , empty
  , near
  , unlink
  , bpTitle
  , bpName
  , bpDesc
  , bpComponents
  , bpGraph
  ) where

import qualified Algebra.Graph as AG
import qualified Control.Lens  as CL
import qualified Domain.Device as DD

data Blueprint
  = Empty
  | Blueprint { _bpName       :: Name
              , _bpTitle      :: String
              , _bpDesc       :: String
              , _bpComponents :: Components
              , _bpGraph      :: AG.Graph DD.Device }
  deriving (Eq, Show)

empty = Empty

defaultBlueprint =
  Blueprint
    { _bpName = ""
    , _bpTitle = ""
    , _bpDesc = ""
    , _bpComponents = []
    , _bpGraph = AG.empty
    }

addDeviceToBp :: DD.Device -> Blueprint -> Blueprint
addDeviceToBp device bp = bp {_bpGraph = added}
  where
    graph = AG.vertex device
    added = AG.overlay graph (_bpGraph bp)

type Components = [Component]

data Component
  = End
  | Hub Name
        Components
  | HubDevice Name
              DD.Device
              Components
  deriving (Eq, Ord, Show)

type Name = String

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

CL.makeLenses ''Blueprint
