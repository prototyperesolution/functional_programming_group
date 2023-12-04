{-# LANGUAGE OverloadedStrings #-}

module Database where


import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow


data Fighter = Fighter{name:: String, wins:: Int, losses:: Int, eloScore:: Int} deriving (Show)

instance FromRow Fighter where 
    fromRow = Fighter <$> field <*> field <*> field <*> field

dbConnection :: IO Connection
dbConnection = do
    conn <- open "fighters.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS fighters (Name TEXT, Wins INT, Losses INT, EloScore INT)"
    return conn


insertFighter :: Connection -> Fighter -> IO()
insertFighter conn fighter = do
    execute conn "INSERT INTO fighters (name, wins, losses, eloScore) VALUES (?, ?, ?, ?)"
        (name fighter, wins fighter, losses fighter, eloScore fighter)

getFighter :: Connection -> String -> IO(Maybe Fighter)
getFighter conn fighterName = do
    queryedFighter <- query conn "SELECT * FROM fighters WHERE name = ?" (Only fighterName)
    case queryedFighter of
        [fighter] -> return (Just fighter)
        _         -> return Nothing 

