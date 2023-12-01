module Types (
    Entry(..),
    Fighter(..),
    Record(..)
) where

import GHC.Generics

data Entry = Entry {
    url_ :: String,
    processed_ :: Bool
} deriving (Show)

data Fighter = Fighter {
    first_name :: String,
    last_name :: String
    -- age
} deriving (Show)

data Record = Record {
    wins :: Int,
    losses :: Int,
    elo_rating :: Int
} deriving (Show)
