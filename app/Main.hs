module Main where

import Database.SQLite.Simple
import Database

main = do
    conn <- dbConnection
    let testFighter = Fighter "Big Steve" 100 50 1500
    insertFighter conn testFighter
    close conn
