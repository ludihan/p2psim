{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)

import Config
import qualified Data.Text.IO as TIO
import Repl
import Types

main :: IO ()
main = do
    args <- getArgs
    cfg <- readConfigFromArgs args
    case cfg of
        Right cfg' -> do
            TIO.putStrLn "Type \"help\" for help"
            repl cfg'
        Left err -> printErrs err
