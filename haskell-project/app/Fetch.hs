{-# LANGUAGE OverloadedStrings #-}


module Fetch (fighter_query, event_query) where

import Types
import Network.Wreq
import Control.Lens
import Data.Text (Text)
import Data.Aeson.Lens (key)
import Parse
import qualified Data.ByteString.Lazy.Char8 as L8


-- | fetches information from the MMA-Stats API about fighters
fighter_query :: Text -> IO L8.ByteString
fighter_query name = do
		let opts = defaults & header "X-RapidAPI-Key" .~ ["f0dac3a241mshf7bf42b2ad9f6dep127712jsnf06c8417ce4d"]
		                    & header "X-RapidAPI-Host" .~ ["mma-stats.p.rapidapi.com"]
		                    & param "name" .~ [name]
		r <- (getWith opts "https://mma-stats.p.rapidapi.com/search")
		pure (r^.responseBody)


-- | uses the Pinnacle Odds API to get all information about upcoming fights and gambling odds
event_query :: IO L8.ByteString
event_query = do
	let opts = defaults & header "X-RapidAPI-Key" .~ ["f0dac3a241mshf7bf42b2ad9f6dep127712jsnf06c8417ce4d"]
		                    & header "X-RapidAPI-Host" .~ ["pinnacle-odds.p.rapidapi.com"]
		                    & param "sport_id" .~ ["8"]
		                    & param "event_type" .~ ["prematch"]
		                    & param "is_have_odds" .~ ["true"]
	r <- (getWith opts "https://pinnacle-odds.p.rapidapi.com/kit/v1/markets")
	pure (r^.responseBody)