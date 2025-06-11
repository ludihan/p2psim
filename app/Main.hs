{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)

import TOML

import qualified Data.Text as T
import Repl
import Types
import Validator

main :: IO ()
main = do
    args <- getArgs
    cfg <- readConfigFromArgs args
    case cfg of
        Right cfg' -> do
            putStrLn "Type \"help\" for help"
            repl cfg'
        Left err -> printErrs err

readConfigFromArgs :: [String] -> IO (Either [Err] Config)
readConfigFromArgs [] = return (Left [Err "no config file"])
readConfigFromArgs (x : _) = do
    readConfig x

readConfig :: FilePath -> IO (Either [Err] Config)
readConfig cfgPath = do
    file <- decodeFile cfgPath
    return $ case file of
        Right cfg -> validateTOML cfg
        Left err -> Left [Err $ T.unpack $ renderTOMLError err]
