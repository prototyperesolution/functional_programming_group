{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Fetch
import Parse
import Types
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text.IO as T
import qualified Data.Map as Map
import Network.Wreq
import Control.Lens
import Data.Text (Text, pack)
import Data.List
import Data.Aeson.Lens (key)
import Control.Monad (forM_)

import Data.Aeson (Value)

--presentFighterDetails :: Text -> [Fighter] -> IO()
--presentFighterDetails selection fighters = do
--	case selection of
	

extractFighterList :: Either String FResults -> IO [Fighter]
extractFighterList parseResults = do
	case parseResults of
		(Left _) -> do
			T.putStrLn "No fighters with that name found"
			return []
		(Right res) -> do
			return (results res)

fighter_mode :: IO ()
fighter_mode = do
	T.putStrLn "Enter fighter name: "
	finput <- pack <$> getLine
	r <- fighter_query finput
	let x = parseResults r
	fighters <- extractFighterList x
	presentFighterResults fighters
	T.putStrLn "For further information on a specific fighter, enter \'f\' followed by the corresponding number"
	T.putStrLn "To search for a different fighter, press s"
	T.putStrLn "To return to the main menu, press \'m\'"
	finput <- pack <$> getLine
	main
	

presentFighterResults :: [Fighter] -> IO ()
presentFighterResults fighters = do
    case fighters of 
    	[] -> fighter_mode
    	_ -> do
		    let nums = [1..length fighters]
		    forM_ (zip fighters nums) $ \(x,y) -> do
		        T.putStr (pack (show y) <> " Name: " <> (name x))
		        case (nickname x) of 
		            "Null" -> T.putStr " No nicknames found"
		            _ -> T.putStr (" Nickname: " <> (nickname x))
		        T.putStrLn "\n"

main :: IO ()
main = do
	fighter_mode
