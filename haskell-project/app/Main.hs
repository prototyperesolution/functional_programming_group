{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Fetch
import Types
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.Wreq
import Control.Lens
import Data.Aeson.Lens (key)

import Data.Map as Map
import Data.Aeson (Value)


type Resp = Response (Map String Value)

main :: IO ()
main = do
	--let opts = defaults & header "X-RapidAPI-Key" .~ ["f0dac3a241mshf7bf42b2ad9f6dep127712jsnf06c8417ce4d"]
	--	                    & header "X-RapidAPI-Host" .~ ["mma-stats.p.rapidapi.com"]
	--	                    & param "name" .~ ["Joe"]
	--r <- asJSON =<< getWith opts "https://mma-stats.p.rapidapi.com/search" :: IO Results
	r <- fighter_query "Joe"
	print r