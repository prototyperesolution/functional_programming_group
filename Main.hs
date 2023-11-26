module Main where

import Database.SQLite.Simple
import Database

main = do
    conn <- dbConnection
    close conn


 