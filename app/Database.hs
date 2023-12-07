{-# LANGUAGE OverloadedStrings #-}

module Database where


import Database.SQLite.Simple
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToField (toField)
import Data.Text
import Types

instance ToRow Fighter where
    toRow f = [ toField (name f)
            , toField (nickname f)
            , toField (division f)
            , toField (wins $ record f)
            , toField (losses $ record f)
            , toField (draws $ record f)
            , toField (status $ bio f)
            , toField (strikes_landed f)
            , toField (strikes_attempted f)
            , toField (takedowns_landed f)
            , toField (takedowns_attempted f)
            , toField (striking_accuracy f)
            , toField (takedown_accuracy f)
            ]

instance FromRow Fighter where
    fromRow = Fighter <$> field <*> field <*> field <*> (Record <$> field <*> field <*> field) <*> (Fighter_Bio <$> field) <*> field <*> field <*> field <*> field <*> field <*> field


dbConnection :: IO Connection
dbConnection = do
    conn <- open "fighters.db"
    return conn


tableExists :: Connection -> String -> IO Bool
tableExists conn tableName = do
  results <- query conn "SELECT name FROM sqlite_master WHERE type='table' AND name=?" (Only tableName) :: IO[Only Text]
  case results of 
    [_] -> return True
    _ -> return False

populateDatabase :: Connection -> [Fighter] -> IO()
populateDatabase conn fighters = do
    execute_ conn "CREATE TABLE IF NOT EXISTS fighters (name TEXT NOT NULL, nickname TEXT, division TEXT, record_wins TEXT NOT NULL, record_losses TEXT NOT NULL, record_draws TEXT NOT NULL, status TEXT, strikes_landed TEXT, strikes_attempted TEXT, takedowns_landed TEXT, takedowns_attempted TEXT, striking_accuracy TEXT, takedown_accuracy TEXT)"
    let query = "INSERT INTO fighters (name, nickname, division, record_wins, record_losses, record_draws, status, strikes_landed, strikes_attempted, takedowns_landed, takedowns_attempted, striking_accuracy, takedown_accuracy) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)" :: Query
    executeMany conn query fighters

queryDatabase :: Connection -> String -> IO [Fighter]
queryDatabase conn fighterName = do
    queryedFighter <- query conn "SELECT * FROM fighters WHERE name LIKE ?" (Only $ "%" <>fighterName<> "%")
    return queryedFighter

