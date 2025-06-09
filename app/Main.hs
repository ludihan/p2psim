{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Console.ANSI
import System.IO (stdout)
import TOML

import Repl
import Types

main :: IO ()
main = do
    args <- getArgs
    cfg <- readConfigFromArgs args
    case cfg of
        Right cfg' -> repl cfg'
        Left err -> printErr err

readConfigFromArgs :: [String] -> IO (Either Err Config)
readConfigFromArgs [] = return (Left (Err "no config file"))
readConfigFromArgs (x : _) = do
    readConfig x

readConfig :: FilePath -> IO (Either Err Config)
readConfig cfgPath = do
    file <- decodeFile cfgPath
    return $ case file of
        Right cfg -> Right cfg
        Left err -> Left $ Err $ renderTOMLError err

printErr :: Err -> IO ()
printErr (Err e) = do
    TIO.putStrLn e
    stdoutSupportsANSI <- hNowSupportsANSI stdout
    TIO.putStr "p2psim: "
    if stdoutSupportsANSI
        then do
            setSGR [SetConsoleIntensity BoldIntensity]
            setSGR [SetColor Foreground Dull Red]
            TIO.putStr "error: "
            setSGR [SetConsoleIntensity BoldIntensity]
            setSGR [SetColor Foreground Vivid White]
            TIO.putStrLn e
            setSGR [Reset]
        else do
            TIO.putStr "error: "
