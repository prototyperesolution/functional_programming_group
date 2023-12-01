{-# LANGUAGE OverloadedStrings #-}

module Database where


import Database.SQLite.Simple

dbConnection = do
    conn <- open "fighters.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS fighters (Name TEXT, Wins INT, Losses INT, EloScore INT)"
    return conn

