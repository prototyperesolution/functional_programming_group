{-# LANGUAGE OverloadedStrings #-}

module Database where


import Database.SQLite.Simple

data Fighter = Fighter{name:: String, wins:: Int, losses:: Int, eloScore:: Int}

dbConnection :: IO Connection
dbConnection = do
    conn <- open "fighters.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS fighters (Name TEXT, Wins INT, Losses INT, EloScore INT)"
    return conn


insertFighter :: Connection -> Fighter -> IO()
insertFighter conn fighter = do
    execute conn "INSERT INTO fighters (name, wins, losses, eloScore) VALUES (?, ?, ?, ?)"
        (name fighter, wins fighter, losses fighter, eloScore fighter)