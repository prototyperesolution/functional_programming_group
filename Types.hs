module Types (
    Entry(..),
    Fighter(..),
    Event(..)
) where

import GHC.Generics

-- web scraping data
data Entry = Entry {
    url_ :: String,
    processed_ :: Bool
} deriving (Show)

-- fighter details
data Fighter = Fighter {
    first_name :: String,
    last_name :: String,
    wins :: Int,
    losses :: Int,
    elo_rating :: Int
} deriving (Show)

-- fighters have many to many relationship to fight events
type Date = (Int, Int, Int)
data Event = Event {
    fightDate :: [Date],
    fighterA :: Fighter,
    fighterB :: Fighter
} deriving (Show)

