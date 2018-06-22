{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Database.Persist.TH
import Data.Aeson
import GHC.Generics

data CollectionPeriod = Daily | Weekly | Monthly
    deriving (Show, Read, Eq, Generic, Enum, Bounded)
instance ToJSON CollectionPeriod
instance FromJSON CollectionPeriod

derivePersistField "CollectionPeriod"

-- newtype Dollars = Int
--     deriving Eq, Ord, Num, Read

-- instance Show Dollars where
--     show d = show (d `div` 100) <> show (d `mod` 100)