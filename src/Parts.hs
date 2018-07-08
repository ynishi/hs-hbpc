{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Parts
    ( 
    Computer(..),
    ComputerType(..),
    Feature(..),
    Part(..),
    Parts,
    Parts.sum,
    Store(..),
    Stores,
    uniqStores
    ) where

import Data.List 

data ComputerType =
    PC |
    NAS deriving(Show,Eq)

data Computer = Computer {
              computerType :: ComputerType,
              parts :: Parts,
              stores :: Stores
              } deriving(Eq, Show)

type Stores = [Store]
data Store = Store String deriving(Eq,Show)

type Parts = [Part]
data Part = Part {
          feature :: Feature,
          price :: Integer
          } deriving(Eq, Show)

data Feature = 
             CPU |
             Disk |
             Memory |
             MotherBoard |
             PCCase |
             PowerUnit |
             Other deriving(Eq, Show)

class Sum a where
    sum :: a -> Integer

-- |
-- Impl sum price of Parts
-- >>> Parts.sum [Part CPU 10000, Part Disk 5000]
-- 15000
instance Sum Parts where
    sum = Prelude.sum . map price

-- |
-- Impl sum price of Computer 
-- >>> Parts.sum $ Computer PC [Part CPU 10000, Part Disk 5000] []
-- 15000
instance Sum Computer where
    sum = Parts.sum . parts

-- |
-- A function uniq Store from Computer 
-- >>> uniqStores $ Computer PC [Part CPU 10000, Part Disk 5000] [Store "example.shop.com"]
-- [Store "example.shop.com"] 
uniqStores :: Computer -> Stores
uniqStores = nub . stores
