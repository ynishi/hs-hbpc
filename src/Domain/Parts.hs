{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Domain.Parts
  ( Computer(..)
  , ComputerType(..)
  , Feature(..)
  , Part(..)
  , Parts
  , Domain.Parts.sum
  , Store(..)
  , Stores
  , uniqStores
  ) where

import           Data.List

data ComputerType
  = PC
  | NAS
  deriving (Show, Eq)

data Computer = Computer
  { computerType :: ComputerType
  , parts        :: Parts
  , stores       :: Stores
  } deriving (Eq, Show)

type Stores = [Store]

newtype Store =
  Store String
  deriving (Eq, Show)

type Parts = [Part]

data Part = Part
  { feature :: Feature
  , price   :: Integer
  , store   :: Store
  } deriving (Eq, Show)

data Feature
  = CPU
  | Disk
  | Memory
  | MotherBoard
  | PCCase
  | PowerUnit
  | Other
  deriving (Eq, Show)

class Sum a where
  sum :: a -> Integer

-- |
-- Impl sum price of Parts
-- >>> Parts.sum [Part CPU 10000 $ Store "", Part Disk 5000 $ Store ""]
-- 15000
instance Sum Parts where
  sum = Prelude.sum . map price

-- |
-- Impl sum price of Computer
-- >>> Parts.sum $ Computer PC [Part CPU 10000 $ Store "", Part Disk 5000 $ Store ""] []
-- 15000
instance Sum Computer where
  sum = Domain.Parts.sum . parts

-- |
-- A function uniq Store from Computer
-- >>> uniqStores $ Computer PC [Part CPU 10000 $ Store "", Part Disk 5000 $ Store "shop.example.com"] [Store "example.shop.com"]
-- [Store "example.shop.com"]
uniqStores :: Computer -> Stores
uniqStores c = nub $ stores c ++ map store (parts c)
