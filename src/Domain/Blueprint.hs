{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Domain.Blueprint
  ( Blueprint(..)
  , Components(..)
  , Component(..)
  , Device(..)
  , link
  , makeDevice
  , near
  , sumPrice
  , unlink
  ) where

import           Control.Lens hiding (element)

data Blueprint = Blueprint
  { _bpName       :: Name
  , _bpTitle      :: String
  , _bpDesc       :: String
  , _bpComponents :: Components
  } deriving (Eq, Show)

type Components = [Component]

type Name = String

data Component
  = End
  | Hub Name
        Components
  | HubDevice Name
              Device
              Components
  deriving (Eq, Show)

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

type Id = String

type Price = Int

type Devices = [Device]

data Device
  = Device { _deviceId          :: Id
           , _devicePrice       :: Price
           , _deviceProductName :: Name
           , _deviceDesc        :: String
           , _deviceContains    :: Devices }
  | DefaultDevice
  deriving (Eq, Show)

makeLenses ''Device

-- |
-- makeDevice
-- >>> makeDevice
-- Device {_deviceId = "", _devicePrice = 0, _deviceProductName = "", _deviceDesc = "", _deviceContains = []}
makeDevice :: Device
makeDevice =
  Device
    { _deviceId = ""
    , _devicePrice = 0
    , _deviceProductName = ""
    , _deviceDesc = ""
    , _deviceContains = []
    }

-- |
-- duplicate
-- >>> duplicate (makeDevice {_deviceId = "device"}) ""
-- Device {_deviceId = "device-1", _devicePrice = 0, _deviceProductName = "", _deviceDesc = "", _deviceContains = []}
duplicate :: Device -> String -> Device
duplicate DefaultDevice _ = DefaultDevice
duplicate device id = device {_deviceId = newId}
  where
    newId =
      if id /= ""
        then id
        else (++ "-1") . _deviceId $ device

-- |
-- sum of price
-- >>> sumPrice DefaultDevice
-- 0
-- >>> sumPrice (Device {_deviceId = "", _devicePrice = 1, _deviceProductName = "", _deviceDesc = "", _deviceContains = [DefaultDevice] })
-- 1
sumPrice :: Device -> Price
sumPrice DefaultDevice = 0
sumPrice Device {_devicePrice = n, _deviceContains = cs} =
  (+ n) . sum . map sumPrice $ cs
