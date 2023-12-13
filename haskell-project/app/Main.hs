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
  	Just x -> locateFighter x listOfFighters
  	Nothing -> case minput of
  		"m" -> main
  		"s" -> fighter_mode conn
  		_ -> do
  			T.putStrLn "Unrecognised input. Returning to main menu"
  			main

event_mode :: Connection -> Connection -> IO()
event_mode eventConn fighterConn = do
	(queryedEvent, fighter1fk, fighter2fk) <- queryEventDatabase eventConn
	let nums = [1..length queryedEvent]
	forM_ (zip queryedEvent nums) $ \(event,eventNum) -> do
		T.putStr(pack (show eventNum) <> " " <> (home event) <> " VS " <> (away event))
		T.putStrLn("\n")
	T.putStrLn "For further information on one of these events, enter the corresponding number"
	T.putStrLn "Or, to return to the main menu, press any other key"
	einput <- getLine
	case (readMaybe einput :: Maybe Int) of
		Just x -> presentEvent x queryedEvent fighter1fk fighter2fk fighterConn
		Nothing -> main

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


invalidSelection :: IO()
invalidSelection = do
	T.putStrLn "Invalid selection. Returning to main menu"
	main

presentEvent :: Int -> [Event] -> [Only Int] -> [Only Int] -> Connection -> IO()
presentEvent x events f1k f2k conn =
	if length events <= 0 then do
		T.putStrLn "No events scheduled. Returning to main menu"
		main
	else
		if x > length events then do
			invalidSelection
		else
			if x <= 0 then do
				invalidSelection
			else do
				let curr_event = (events !! (x-1))
				T.putStrLn((home curr_event) <> " VS " <> (away curr_event) <> "\n")
				T.putStrLn("Odds (via Pinnacle Sports):")
				T.putStrLn((home curr_event) <> ": " <> pack (show (homebet (money_line (num_0 (periods curr_event))))))
				T.putStrLn((away curr_event) <> ": " <> pack (show (awaybet (money_line (num_0 (periods curr_event))))))
				T.putStrLn("\n")
				T.putStrLn("Date and Time: " <> (starts curr_event))
				fighter1 <- fkFighterDatabase conn (fromOnly (f1k !! (x-1)))
				fighter2 <- fkFighterDatabase conn (fromOnly (f2k !! (x-1)))
				case fighter1 of
					[f1] -> presentSingleFighter f1
					_ -> do 
						T.putStrLn "fighter not recognised. Returning to main menu"
						main
				case fighter2 of
					[f2] -> presentSingleFighter f2
					_ -> do
						T.putStrLn "fighter not recognised. Returning to main menu"
						main
				main

presentSingleFighter :: Fighter -> IO ()
presentSingleFighter curr_fighter = do
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
	T.putStrLn("\n")

locateFighter :: Int -> [Fighter] -> IO ()
locateFighter x list =
    if length list <= 0 then do
		T.putStrLn "No fighters selected. Returning to main menu"
		main
	else
    	if x > length list then do 
			invalidSelection
		else
		    if x <= 0 then do
				invalidSelection
		    else do
		    	let curr_fighter = (list !! (x-1))
		    	presentSingleFighter curr_fighter
		    	T.putStrLn "Returning to main menu"
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
  	"2" -> event_mode eventConn fighterConn
  	_ -> main
  close fighterConn
  close eventConn

