{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Domain.Device
  ( Name
  , Device(..)
  , deviceName
  , deviceDesc
  , defaultDevice
  , deviceIfaces
  , hasProtocol
  , hasTCPIP
  ) where

import qualified Control.Lens as CL
import qualified Data.List    as List

type Name = String

data Device
  = Device { _deviceName   :: Name
           , _deviceDesc   :: String
           , _deviceIfaces :: [String] }
  | Empty
  deriving (Eq, Ord, Show)

CL.makeLenses ''Device

empty :: Device
empty = Empty

-- |
-- defaultDevice
-- >>> defaultDevice
-- Device {_deviceName = "", _deviceDesc = "", _deviceIfaces = []}
defaultDevice :: Device
defaultDevice = Device {_deviceName = "", _deviceDesc = "", _deviceIfaces = []}

-- |
-- has TCPIP
-- >>> hasTCPIP defaultDevice
-- False
-- >>> hasTCPIP $ defaultDevice CL.& deviceIfaces CL..~ ["TCPIP"]
-- True
hasTCPIP :: Device -> Bool
hasTCPIP = hasProtocol "TCPIP"

hasProtocol :: String -> Device -> Bool
hasProtocol protocol device = List.elem protocol $ device CL.^. deviceIfaces
