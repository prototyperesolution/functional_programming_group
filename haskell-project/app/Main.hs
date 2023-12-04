{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Fetch
import Parse
import Types
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text.IO as T
import Network.Wreq
import Control.Lens
import Data.Text (Text, pack)
import Data.Aeson.Lens (key)

import Data.Aeson (Value)


main :: IO ()
main = do
	T.putStrLn "Enter fighter name: "
	name <- pack <$> getLine
	r <- fighter_query $  name
	let x = parseResults r
	presentResults x
	main