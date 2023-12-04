{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


module Types (
    Fighter (..),
    Event (..),
    Record (..),
    Language(..),
    FResults(..),
    Fighter_Bio(..)
) where

import Data.Text (Text)
import Data.Aeson
import GHC.Generics

data FResults = FResults{
    results :: [Fighter]
} deriving (Show, Generic)


data Language = Language{
    code :: Text,
    lname :: Text
} deriving (Show, Generic)
instance FromJSON Language

data Fighter_Bio = Fighter_Bio{
    status :: Text
} deriving (Show, Generic)

data Record = Record{
    wins :: Text,
    losses :: Text,
    draws :: Text
} deriving (Show, Generic)



data Fighter = Fighter{
    name :: Text,
    nickname :: Text,
    division :: Text,
    record :: Record,
    bio :: Fighter_Bio,
    strikes_landed :: Text,
    strikes_attempted :: Text,
    takedowns_landed :: Text,
    takedowns_attempted :: Text,
    striking_accuracy :: Text,
    takedown_accuracy :: Text
} deriving (Show, Generic)
 

data Event = Event{
    placeholder :: Text
}