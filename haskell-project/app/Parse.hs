{-# LANGUAGE OverloadedStrings #-}
module Parse (parseResults) where

import Data.List
import Types
import Data.Aeson
import Control.Monad (forM_)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text.IO as T

--Annoyingly all the keys in the API response are capitalised

--renaming the fields for the fighter type
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
renameFields other = other

--replacing nulls

customOptions = defaultOptions{
    fieldLabelModifier = renameFields,
    omitNothingFields = True
}

instance FromJSON Fighter where
    parseJSON = genericParseJSON customOptions

instance FromJSON Record where
    parseJSON = genericParseJSON customOptions

instance FromJSON Fighter_Bio where
    parseJSON = genericParseJSON customOptions

instance FromJSON FResults

parseResults :: L8.ByteString -> Either String FResults
parseResults json = eitherDecode json :: Either String FResults




