module Main where

import Database.SQLite.Simple
import Database

main = do
    conn <- dbConnection
    let testFighter = Fighter "Big Steve" 100 50 1500
    let testFighter2 = Fighter "John" 75 40 1200
    insertFighter conn testFighter
    insertFighter conn testFighter2
    maybeFighter <- getFighter conn "Big Steve"
    case maybeFighter of
        Just fighter -> putStrLn $ show fighter
        Nothing -> putStrLn "Cannot find fighter"
    close conn

