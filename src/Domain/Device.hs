{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Domain.Device
  ( Id
  , Price
  , Name
  , Devices(..)
  , Device(..)
  , deviceId
  , devicePrice
  , deviceProductName
  , deviceDesc
  , deviceContains
  , defaultDevice
  , deviceIfaces
  , hasProtocol
  , hasTCPIP
  , sumPrice
  ) where

import qualified Control.Lens as CL
import qualified Data.List    as List

type Id = String

type Name = String

type Price = Int

type Devices = [Device]

data Device
  = Device { _deviceId          :: Id
           , _devicePrice       :: Price
           , _deviceProductName :: Name
           , _deviceDesc        :: String
           , _deviceContains    :: Devices
           , _deviceIfaces      :: [String] }
  | DefaultDevice
  deriving (Eq, Ord, Show)

CL.makeLenses ''Device

-- |
-- defaultDevice
-- >>> defaultDevice
-- Device {_deviceId = "", _devicePrice = 0, _deviceProductName = "", _deviceDesc = "", _deviceContains = [], _deviceIfaces = []}
defaultDevice :: Device
defaultDevice =
  Device
    { _deviceId = ""
    , _devicePrice = 0
    , _deviceProductName = ""
    , _deviceDesc = ""
    , _deviceContains = []
    , _deviceIfaces = []
    }

-- |
-- duplicate
-- >>> duplicate (defaultDevice CL.& deviceId CL..~  "device") ""
-- Device {_deviceId = "device-1", _devicePrice = 0, _deviceProductName = "", _deviceDesc = "", _deviceContains = [], _deviceIfaces = []}
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
-- >>> sumPrice defaultDevice
-- 0
-- >>> sumPrice $ defaultDevice CL.& devicePrice CL..~ 1
-- 1
sumPrice :: Device -> Price
sumPrice DefaultDevice = 0
sumPrice Device {_devicePrice = n, _deviceContains = cs} =
  (+ n) . sum . map sumPrice $ cs

-- |
-- has TCPIP
-- >>> hasTCPIP defaultDevice
-- False
-- >>> hasTCPIP $ defaultDevice CL.& deviceIfaces CL..~ ["tcpip"]
-- True
hasTCPIP :: Device -> Bool
hasTCPIP = hasProtocol "tcpip"

hasProtocol :: String -> Device -> Bool
hasProtocol protocol device = List.elem protocol $ device CL.^. deviceIfaces
