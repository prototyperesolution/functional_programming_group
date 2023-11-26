{-# LANGUAGE OverloadedStrings #-}

module Database where


import Database.SQLite.Simple

dbConnection = do
    conn <- open "fighters.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS fighters (name TEXT,age INT, wins INT, losses INT)"
    return conn

