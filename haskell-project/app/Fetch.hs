{-# LANGUAGE OverloadedStrings #-}

module Fetch (getLanguages, fighter_query) where

import Types
import Network.Wreq
import Control.Lens
import Data.Text (Text)
import Data.Aeson.Lens (key)
import Parse
import qualified Data.ByteString.Lazy.Char8 as L8

getLanguages:: IO [Language]
getLanguages = do
	rsp <- asJSON =<< get "https://libretranslate.com/languages"
	pure (rsp ^. responseBody)

fighter_query :: Text -> IO FResults
fighter_query name = do
		let opts = defaults & header "X-RapidAPI-Key" .~ ["f0dac3a241mshf7bf42b2ad9f6dep127712jsnf06c8417ce4d"]
		                    & header "X-RapidAPI-Host" .~ ["mma-stats.p.rapidapi.com"]
		                    & param "name" .~ [name]
		r <- asJSON =<< getWith opts "https://mma-stats.p.rapidapi.com/search"
		pure (r^.responseBody)