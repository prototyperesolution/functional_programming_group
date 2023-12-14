{-# LANGUAGE OverloadedStrings #-}
module Parse (parseResults, parseEvents) where

---import Data.List
import Types
import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import Control.Monad (forM_)
import Text.Read
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text.IO as T


--Annoyingly all the keys in the API response are capitalised

-- |renaming the fields for the different types
renameFields "name" = "Name"
renameFields "nickname" = "Nickname"
renameFields "division" = "Division Title"
renameFields "record" = "Division Body"
renameFields "bio" = "Bio Data"
renameFields "strikes_landed" = "Sig. Strikes Landed"
renameFields "strikes_attempted" = "Sig. Strikes Attempted"
renameFields "takedowns_attempted" = "Takedowns Attempted"
renameFields "takedowns_landed" = "Takedowns Landed"
renameFields "striking_accuracy" = "Striking accuracy"
renameFields "takedown_accuracy" = "Takedown Accuracy"

--renaming the fields for the record type
renameFields "wins" = "Wins"
renameFields "losses" = "Losses"
renameFields "draws" = "Draws"

--renaming the fields for the bio type
renameFields "status" = "Status"

--renaming fields for the money_line type
renameFields "homebet" = "home"
renameFields "awaybet" = "away"
renameFields other = other

--replacing nulls

customOptions = defaultOptions{
    fieldLabelModifier = renameFields,
    omitNothingFields = True
    }

-- |parsing Fighter objects
instance FromJSON Fighter where
    parseJSON = genericParseJSON customOptions

-- |parsing Record objects, taking "Maybe Text" values to Maybe Int values
instance FromJSON Record where
    parseJSON (Object obj) = do
        winVal <- obj .: "Wins"
        lossVal <- obj .: "Losses"
        drawsVal <- obj .: "Draws"
        let winNum = (readMaybe winVal :: Maybe Int)
        let lossNum = (readMaybe lossVal :: Maybe Int)
        let drawsNum = (readMaybe drawsVal :: Maybe Int)
        return $ Record winNum lossNum drawsNum

-- |parsing Fighter_Bio objects
instance FromJSON Fighter_Bio where
    parseJSON = genericParseJSON customOptions

-- |parsing Money_Line objects, with a default Money_Line if the JSON object is null
instance FromJSON Money_Line where
    parseJSON Null = return $ Money_Line 0.0 0.0
    parseJSON (Object obj) = do
        homeVal <- obj .: "home"
        awayVal <- obj .: "away"
        return $ Money_Line homeVal awayVal

-- | parsing Event objects
instance FromJSON Event where
    parseJSON = genericParseJSON customOptions

instance FromJSON Periods
instance FromJSON Num_0
instance FromJSON Events
instance FromJSON FResults


-- |Uses eitherDecode to parse fighters into either a list of fighters (FResults) or a string
parseResults :: L8.ByteString -> Either String FResults
parseResults json = eitherDecode json :: Either String FResults

-- |Uses eitherDecode to parse Events into either a list of events (Events) or a string
parseEvents :: L8.ByteString -> Either String Events
parseEvents json = eitherDecode json :: Either String Events
