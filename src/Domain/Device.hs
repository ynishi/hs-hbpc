{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Domain.Device
  ( Id
  , Price
  , Name
  , Devices(..)
  , Device(..)
  , defaultDevice
  , sumPrice
  ) where

import           Control.Lens hiding (element)

type Id = String

type Name = String

type Price = Int

type Devices = [Device]

data Device
  = Device { _deviceId          :: Id
           , _devicePrice       :: Price
           , _deviceProductName :: Name
           , _deviceDesc        :: String
           , _deviceContains    :: Devices }
  | DefaultDevice
  deriving (Eq, Ord, Show)

makeLenses ''Device

-- |
-- defaultDevice
-- >>> defaultDevice
-- Device {_deviceId = "", _devicePrice = 0, _deviceProductName = "", _deviceDesc = "", _deviceContains = []}
defaultDevice :: Device
defaultDevice =
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
