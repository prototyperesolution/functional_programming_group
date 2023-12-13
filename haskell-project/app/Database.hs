{-# LANGUAGE OverloadedStrings #-}

module Database where


import Database.SQLite.Simple
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField (toField)
import Control.Monad (forM_)
import Data.Text (Text)
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

instance ToRow Event where
    toRow f =
        [ toField (starts f)
        , toField (home f)
        , toField (away f)
        , toField (homebet $ money_line $ num_0 $ periods f)
        , toField (awaybet $ money_line $ num_0 $ periods f)
        ]

instance FromRow Event where
    fromRow = Event <$> field <*> field <*> field <*> (Periods <$> (Num_0 <$> (Money_Line <$> field <*> field)))

--doing pattern matching cos we have an ID field when we get back the field from the DB

instance FromRow Fighter where
    fromRow = Fighter <$> field <*> field <*> field <*> (Record <$> field <*> field <*> field) <*> (Fighter_Bio <$> field) <*> field <*> field <*> field <*> field <*> field <*> field

dbConnection :: String -> IO Connection
dbConnection target = do
    conn <- open target
    return conn


tableExists :: Connection -> String -> IO Bool
tableExists conn tableName = do
  results <- query conn "SELECT name FROM sqlite_master WHERE type='table' AND name=?" (Only tableName) :: IO[Only Text]
  case results of 
    [_] -> return True
    _ -> return False

populateFighterDatabase :: Connection -> [Fighter] -> IO()
populateFighterDatabase conn fighters = do
    execute_ conn "CREATE TABLE IF NOT EXISTS fighters (\
        \id INTEGER PRIMARY KEY AUTOINCREMENT\
        \, name TEXT NOT NULL\
        \, nickname TEXT\
        \, division TEXT\
        \, record_wins TEXT NOT NULL\
        \, record_losses TEXT NOT NULL\
        \, record_draws TEXT NOT NULL\
        \, status TEXT, strikes_landed TEXT\
        \, strikes_attempted TEXT\
        \, takedowns_landed TEXT\
        \, takedowns_attempted TEXT\
        \, striking_accuracy TEXT\
        \, takedown_accuracy TEXT\
        \)"
    let insert = "INSERT INTO fighters (\
        \name, nickname, division, record_wins\
        \, record_losses, record_draws, status\
        \, strikes_landed, strikes_attempted\
        \, takedowns_landed, takedowns_attempted\
        \, striking_accuracy, takedown_accuracy\
        \) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)" :: Query
    executeMany conn insert fighters



populateEventDatabase :: Connection -> Connection -> [Event] -> IO()
populateEventDatabase eventConn fighterConn events = do
    execute_ eventConn "CREATE TABLE IF NOT EXISTS events (\
        \id INTEGER PRIMARY KEY AUTOINCREMENT\
        \, starts TEXT NOT NULL\
        \, fighter_1 TEXT\
        \, fighter_2 TEXT\
        \, fighter_1_odds FLOAT NOT NULL\
        \, fighter_2_odds FLOAT NOT NULL\
        \, fighter_1_fk INTEGER\
        \, fighter_2_fk INTEGER\
        \)"
    let insert = "INSERT INTO events (\
        \starts, fighter_1, fighter_2,\
        \fighter_1_odds, fighter_2_odds\
        \) VALUES (?, ?, ?, ?, ?)" :: Query
    executeMany eventConn insert events
    ---populating the foreign keys
    let nums = [1.. length events]
    forM_ (zip events nums) $ \(event, eventNum) -> do
        fighter1fk <- query fighterConn "SELECT id FROM fighters WHERE name LIKE ?" (Only $ "%" <>(home event)<> "%") :: IO [Only Int]
        fighter2fk <- query fighterConn "SELECT id FROM fighters WHERE name LIKE ?" (Only $ "%" <>(away event)<> "%") :: IO [Only Int]
        case fighter1fk of
            [Only id1] -> executeNamed eventConn "UPDATE events SET fighter_1_fk = :val WHERE id = :id" [":val" := id1, ":id" := eventNum]
            _ -> executeNamed eventConn "UPDATE events SET fighter_1_fk = 10 WHERE id = :id" [":id" := eventNum]
        case fighter2fk of
            [Only id2] -> executeNamed eventConn "UPDATE events SET fighter_2_fk = :val WHERE id = :id" [":val" := id2, ":id" := eventNum]
            _ -> executeNamed eventConn "UPDATE events SET fighter_2_fk = 10 WHERE id = :id" [":id" := eventNum]


queryFighterDatabase :: Connection -> Text -> IO [Fighter]
queryFighterDatabase conn fighterName = do
    queryedFighter <- query conn "SELECT name, nickname, division, record_wins, \
                                \record_losses, record_draws, status, strikes_landed, \
                                \strikes_attempted, takedowns_landed, takedowns_attempted, \
                                \striking_accuracy, takedown_accuracy FROM fighters WHERE name LIKE ?" (Only $ "%" <>fighterName<> "%")
    return queryedFighter

fkFighterDatabase :: Connection -> Int -> IO [Fighter]
fkFighterDatabase conn fk = do
    fighter <- queryNamed conn "SELECT name, nickname, division, record_wins, \
                                \record_losses, record_draws, status, strikes_landed, \
                                \strikes_attempted, takedowns_landed, takedowns_attempted, \
                                \striking_accuracy, takedown_accuracy FROM fighters WHERE id = :id" [":id":=fk]
    return fighter

queryEventDatabase :: Connection -> IO ([Event], [Only Int], [Only Int])
queryEventDatabase conn = do
    queryedEvent <- query_ conn "SELECT starts, fighter_1, fighter_2, fighter_1_odds, fighter_2_odds FROM events"
    fighter1fk <- query_ conn "SELECT fighter_1_fk FROM events" :: IO [Only Int]
    fighter2fk <- query_ conn "SELECT fighter_2_fk FROM events" :: IO [Only Int]
    return (queryedEvent, fighter1fk, fighter2fk)     
