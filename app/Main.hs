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
import GHC.Generics (Constructor(conName))

-- testing branch

extractFighterList :: Either String FResults -> [Fighter]
extractFighterList (Left _) = []
extractFighterList (Right res) = results res

fighter_mode :: Connection -> IO ()
fighter_mode conn = do
  -- T.putStrLn "Enter fighter name: "
  -- finput <- pack <$> getLine
  --T.putStrLn "For further information on a specific fighter, enter \'f\' followed by the corresponding number"
  --T.putStrLn "To search for a different fighter, press s"
  --T.putStrLn "To return to the main menu, press \'m\'"
  T.putStrLn "Search for a fighter: "
  finput <- getLine
  listOfFighters <- queryDatabase conn finput
  mapM_ print listOfFighters
  putStrLn $ "Number of fighers found: " ++ show(length listOfFighters)
  main

initialiseDB :: Connection -> IO ()
initialiseDB conn = do
  -- conn <- dbConnection
  r <- fighter_query $ pack " "
  let x = parseResults r
  let fighters = extractFighterList x
  --presentFighterResults fighters
  populateDatabase conn fighters


{-
presentFighterResults :: [Fighter] -> IO ()
presentFighterResults fighters = do
  case fighters of
    [] -> do
      T.putStrLn ("No fighters found with that name")
      --fighter_mode
    _ -> do
      let nums = [1 .. length fighters]
      forM_ (zip fighters nums) $ \(x, y) -> do
        T.putStr (pack (show y) <> " Name: " <> (name x))
        case (nickname x) of
          "Null" -> T.putStr " No nicknames found"
          _ -> T.putStr (" Nickname: " <> (nickname x))
        T.putStrLn "\n"
-}

main :: IO ()
main = do
  conn <- dbConnection
  exists <- tableExists conn "fighters"
  if exists then putStrLn "The table does exist" else initialiseDB conn
  fighter_mode conn
  close conn

  