{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Domain.Buying
  ( Buying(..)
  , estimate
  , defaultBuying
  , buyingPrice
  , buyingDevice
  , buyingShop
  , buyingAt
  ) where

import qualified Control.Lens  as CL
import qualified Data.List     as List
import qualified Domain.Device as D

data Buying = Buying
  { _buyingPrice  :: Int
  , _buyingDevice :: D.Device
  , _buyingShop   :: String
  , _buyingAt     :: String
  } deriving (Eq, Ord, Show)

CL.makeLenses ''Buying

defaultBuying :: Buying
defaultBuying =
  Buying
    { _buyingPrice = 0
    , _buyingDevice = D.defaultDevice
    , _buyingShop = ""
    , _buyingAt = ""
    }

estimate :: [Buying] -> Int
estimate = sum . map _buyingPrice
