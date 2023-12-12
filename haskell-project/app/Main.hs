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
  listOfFighters <- queryDatabase conn finput
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



initialiseDB :: Connection -> IO ()
initialiseDB conn = do
  r <- fighter_query $ pack " "
  let x = parseResults r
  let fighters = extractFighterList x
  populateDatabase conn fighters

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
		    	let curr_fighter = (list !! x)
		    	T.putStrLn ("Name: " <> (name curr_fighter))
		    	T.putStrLn ("Nickname: " <> (nickname curr_fighter))
		    	case division curr_fighter of
		    		Just x -> T.putStrLn ("Division: "<> x)
		    	case wins (record curr_fighter) of
		    		Just x -> T.putStrLn ("Wins: "<> x)
		    	case losses (record curr_fighter) of
		    		Just x -> T.putStrLn ("Losses: "<> x)
		    	case draws (record curr_fighter) of
		    		Just x -> T.putStrLn ("Draws: "<> x)
		    	case status (bio curr_fighter) of
		    		Just x -> T.putStrLn ("Active: "<> x)
		    	case strikes_landed curr_fighter of
		    		Just x -> T.putStrLn ("Significant Strikes Landed: "<> x)
		    	case strikes_attempted curr_fighter of
		    		Just x -> T.putStrLn ("Strikes Attempted: "<> x)
		    	case takedowns_landed curr_fighter of
		    		Just x -> T.putStrLn ("Takedowns Landed: "<> x)
		    	case takedowns_attempted curr_fighter of
		    		Just x -> T.putStrLn ("Takedowns Attempted: "<> x)
		    	case striking_accuracy curr_fighter of
		    		Just x -> T.putStrLn ("Striking Accuracy: "<> x)
		    	case takedown_accuracy curr_fighter of
		    		Just x -> T.putStrLn ("Takedown Accuracy: "<> x)
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
  conn <- dbConnection
  exists <- tableExists conn "fighters"
  if not exists then initialiseDB conn else T.putStrLn("")
  T.putStrLn "Welcome to Group 26 MMA prediction and gambling advice"
  fighter_mode conn
  close conn

