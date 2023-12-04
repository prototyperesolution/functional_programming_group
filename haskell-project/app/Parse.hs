module Parse (parseResults) where

import Data.List
import Types
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8

--Annoyingly all the keys in the API response are capitalised

--renaming the fields for the fighter type
renameFields "name" = "Name"
renameFields "nickname" = "Nickname"
renameFields "division" = "Division Title"
renameFields "record" = "Division Body"
renameFields "bio" = "Bio Data"
renameFields "strikes_landed" = "Sig. Strikes Landed"
renameFields "strikes_attempted" = "Sig. Strikes Attempted"
renameFields "takedowns_landed" = "Takedowns Landed"
renameFields "striking_accuracy" = "Striking accuracy"
renameFields "takedown_accuracy" = "Takedown Accuracy"

--renaming the fields for the record type
renameFields "wins" = "Wins"
renameFields "losses" = "Losses"
renameFields "draws" = "Draws"

--renaming the fields for the bio type
renameFields "status" = "Status"

customOptions = defaultOptions{
    fieldLabelModifier = renameFields
}

instance FromJSON Fighter where
    parseJSON = genericParseJSON customOptions

instance FromJSON Record where
    parseJSON = genericParseJSON customOptions

instance FromJSON Fighter_Bio where
    parseJSON = genericParseJSON customOptions

instance FromJSON FResults

parseResults :: 


