{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


module Types (
    Fighter (..),
    Record (..),
    FResults(..),
    Fighter_Bio(..),
    Money_Line(..),
    Num_0(..),
    Periods(..),
    Event(..),
    Events(..)
) where

import Data.Text (Text)
import Data.Aeson
import GHC.Generics

-- | list of fighters, this datatype only exists for parsing JSON
data FResults = FResults{
    results :: [Fighter]
} deriving (Show, Generic)

-- | Type containing the status of a fighter. Nested data structure due to how the API returns data
data Fighter_Bio = Fighter_Bio{
    status :: Maybe Text
} deriving (Show, Generic)


-- | Type containing the record of the fighter. Nested data structure due to how the API returns data
-- contains wins, losses, draws
data Record = Record{
    wins :: Maybe Int,
    losses :: Maybe Int,
    draws :: Maybe Int
} deriving (Show, Generic, Eq, Ord)


-- | Type for Fighters, containing information about them and nested data types of record and bio
data Fighter = Fighter{
    name :: Text,
    nickname :: Text,
    division :: Maybe Text,
    record :: Record,
    bio :: Fighter_Bio,
    strikes_landed :: Maybe Text,
    strikes_attempted :: Maybe Text,
    takedowns_landed :: Maybe Text,
    takedowns_attempted :: Maybe Text,
    striking_accuracy :: Maybe Text,
    takedown_accuracy :: Maybe Text
} deriving (Show, Generic)


-- | Type for storing gambling odds, with odds for both fighters
data Money_Line = Money_Line{
    homebet :: Float,
    awaybet :: Float
} deriving (Show, Generic)

-- | Type for dealing with nested data structure of Pinnacle Odds API response (contains Money_Line type)
data Num_0 = Num_0{
    money_line :: Money_Line
} deriving (Show, Generic)

-- | Type for dealing with nested data structure of Pinnacle Odds API response (contains Num_0 type)
data Periods = Periods{
    num_0 :: Num_0
} deriving (Show, Generic)

-- | Type for handling events, contains information about start, who is competing, and gambling odds
data Event = Event{
    starts :: Text,
    home :: Text,
    away :: Text,
    periods :: Periods
} deriving (Show, Generic)

-- | Type for list of events
data Events = Events{
    events :: [Event]
} deriving (Show, Generic)
