{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Lens
import qualified Control.Lens.Internal.Deque as T
import Control.Monad (forM_)
import Data.Aeson (Value)
import Data.Aeson.Lens (key)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.List
import qualified Data.Map as Map
import Data.Text (Text, pack)
import qualified Data.Text.IO as T
import Fetch
import Network.Wreq
import Parse
import Types
import Database.SQLite.Simple
import Database
import Text.Read
import GHC.Generics (Constructor(conName))

-- testing branch

extractFighterList :: Either String FResults -> [Fighter]
extractFighterList (Left _) = []
extractFighterList (Right res) = results res

extractEventList :: Either String Events -> [Event]
extractEventList (Left _) = []
extractEventList (Right res) = events res

fighter_mode :: Connection -> IO ()
fighter_mode conn = do
  -- T.putStrLn "Enter fighter name: "
  -- finput <- pack <$> getLine
  T.putStrLn "Search for a fighter: "
  finput <- pack <$> getLine
  listOfFighters <- queryFighterDatabase conn finput
  presentFighterResults conn listOfFighters
  T.putStrLn "For further information on a specific fighter, enter their corresponding number"
  T.putStrLn "To search for a different fighter, press s"
  T.putStrLn "To return to the main menu, press \'m\'"
  minput <- getLine
  case (readMaybe minput :: Maybe Int) of
  	Just x -> presentSingleFighter x listOfFighters
  	Nothing -> case minput of
  		"m" -> main
  		"s" -> fighter_mode conn
  		_ -> do
  			T.putStrLn "Unrecognised input. Returning to main menu"
  			main



initialiseFighterDB :: Connection -> IO ()
initialiseFighterDB conn = do
  r <- fighter_query $ pack " "
  let x = parseResults r
  let fighters = extractFighterList x
  populateFighterDatabase conn fighters


initialiseEventDB :: Connection -> Connection -> IO ()
initialiseEventDB eventConn fighterConn = do
  r <- event_query
  let x = parseEvents r
  let events = extractEventList x
  populateEventDatabase eventConn fighterConn events

presentSingleFighter :: Int -> [Fighter] -> IO ()
presentSingleFighter x list =
    if length list <= 0 then do
		T.putStrLn "No fighters selected. Returning to main menu"
		main
	else
    	if x > length list then do 
			T.putStrLn "Invalid selection. Returning to main menu"
			main
		else
		    if x <= 0 then do
				T.putStrLn "Invalid selection. Returning to main menu"
				main
		    else do
		    	let curr_fighter = (list !! (x-1))
		    	T.putStrLn ("Name: " <> (name curr_fighter))
		    	T.putStrLn ("Nickname: " <> (nickname curr_fighter))
		    	case division curr_fighter of
		    		Just x -> T.putStrLn ("Division: "<> x)
		    		_ -> T.putStrLn(" ")
		    	case wins (record curr_fighter) of
		    		Just x -> T.putStrLn ("Wins: "<> x)
		    		_ -> T.putStrLn(" ")
		    	case losses (record curr_fighter) of
		    		Just x -> T.putStrLn ("Losses: "<> x)
		    		_ -> T.putStrLn(" ")
		    	case draws (record curr_fighter) of
		    		Just x -> T.putStrLn ("Draws: "<> x)
		    		_ -> T.putStrLn(" ")
		    	case status (bio curr_fighter) of
		    		Just x -> T.putStrLn ("Status: "<> x)
		    		_ -> T.putStrLn(" ")
		    	case strikes_landed curr_fighter of
		    		Just x -> T.putStrLn ("Significant Strikes Landed: "<> x)
		    		_ -> T.putStrLn(" ")
		    	case strikes_attempted curr_fighter of
		    		Just x -> T.putStrLn ("Strikes Attempted: "<> x)
		    		_ -> T.putStrLn(" ")
		    	case takedowns_landed curr_fighter of
		    		Just x -> T.putStrLn ("Takedowns Landed: "<> x)
		    		_ -> T.putStrLn(" ")
		    	case takedowns_attempted curr_fighter of
		    		Just x -> T.putStrLn ("Takedowns Attempted: "<> x)
		    		_ -> T.putStrLn(" ")
		    	case striking_accuracy curr_fighter of
		    		Just x -> T.putStrLn ("Striking Accuracy: "<> x)
		    		_ -> T.putStrLn(" ")
		    	case takedown_accuracy curr_fighter of
		    		Just x -> T.putStrLn ("Takedown Accuracy: "<> x)
		    		_ -> T.putStrLn(" ")
		    	T.putStrLn ("Returning to main menu")
		    	main

presentFighterResults :: Connection -> [Fighter] -> IO ()
presentFighterResults conn fighters = do
    case fighters of 
    	[] -> do
    		T.putStrLn("No fighters found with that name")
    		fighter_mode conn
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
  fighterConn <- dbConnection "fighters.db"
  eventConn <- dbConnection "events.db"
  fightersExists <- tableExists fighterConn "fighters"
  eventsExists <- tableExists eventConn "events"
  if not fightersExists then initialiseFighterDB fighterConn else T.putStrLn("")
  if not eventsExists then initialiseEventDB eventConn fighterConn else T.putStrLn("")
  T.putStrLn "Welcome to Group 26 MMA prediction and gambling advice"
  T.putStrLn "Select from one of the following options: \n 1. Fighter Info Search \n 2. Upcoming Events and Odds"
  minput <- getLine
  case minput of
  	"1" -> fighter_mode fighterConn
  	--"2" -> event_mode eventConn
  	_ -> main
  close fighterConn
  close eventConn

