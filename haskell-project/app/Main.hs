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


extractFighterList :: Either String FResults -> [Fighter]
extractFighterList (Left _) = []
extractFighterList (Right res) = results res

extractEventList :: Either String Events -> [Event]
extractEventList (Left _) = []
extractEventList (Right res) = events res

fighter_mode :: IO ()
fighter_mode = do
	T.putStrLn "Enter fighter name: "
	finput <- pack <$> getLine
	r <- fighter_query finput
	let x = parseResults r
	let fighters = extractFighterList x
	presentFighterResults fighters
	T.putStrLn "For further information on a specific fighter, enter the corresponding number"
	T.putStrLn "To search for a different fighter, press \'s\'"
	T.putStrLn "To return to the main menu, press \'m\'"
	finput <- pack <$> getLine
	case finput of
		"m" -> main
		"s" -> fighter_mode

event_mode :: IO ()
event_mode = do
	e <- event_query
	let x = parseEvents e
	let events = extractEventList x
	presentEvents events
	main

	

presentFighterResults :: [Fighter] -> IO ()
presentFighterResults fighters = do
    case fighters of 
    	[] -> do
    		T.putStrLn("No fighters found with that name")
    		fighter_mode
    	_ -> do
		    let nums = [1..length fighters]
		    forM_ (zip fighters nums) $ \(x,y) -> do
		        T.putStr (pack (show y) <> " Name: " <> (name x))
		        case (nickname x) of 
		            "Null" -> T.putStr " No nicknames found"
		            _ -> T.putStr (" Nickname: " <> (nickname x))
		        T.putStrLn "\n"

presentEvents :: [Event] -> IO ()
presentEvents events = do
    case events of
        [] -> do
            T.putStrLn("No upcoming events found")
            main
        _ -> do
        	let nums = [1..length events]
        	forM_ (zip events nums) $ \(x,y) -> do
        		T.putStrLn (pack (show y) <> " " <> (home x) <> " vs " <> (away x))
        		T.putStrLn ("Date/Time: "<> (starts x))
        		T.putStrLn ("Odds (Via pinnacle odds API) ")
        		let ml = money_line (num_0 (periods x))
        		case ml of
        			Nothing -> T.putStrLn "No odds available \n"
        			(Just y) -> do
        				T.putStr((home x) <> ": " <> pack (show (homebet y)) <> " ")
        				T.putStr((away x) <> ": " <> pack (show (awaybet y)))
        				T.putStrLn("\n")

main :: IO ()
main = do
	T.putStrLn "Select option: 1. for fighter search 2. for event search 3. for fighter comparison"
	input <- pack <$> getLine
	case input of
		"1" -> fighter_mode
		"2" -> event_mode
		"3" -> main
