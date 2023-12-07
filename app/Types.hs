{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


module Types (
    Fighter (..),
    Event (..),
    Record (..),
    FResults(..),
    Fighter_Bio(..)
) where

import Data.Text (Text)
import Data.Aeson
import GHC.Generics

data FResults = FResults{
    results :: [Fighter]
} deriving (Show, Generic)

data Fighter_Bio = Fighter_Bio{
    status :: Maybe Text
} deriving (Show, Generic)

data Record = Record{
    wins :: Maybe Text,
    losses :: Maybe Text,
    draws :: Maybe Text
} deriving (Show, Generic)



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
 

data Event = Event{
    placeholder :: Text
}