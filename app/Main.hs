{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)

import Config
import Control.Monad
import qualified Data.Text.IO as TIO
import Repl
import System.Console.ANSI
import System.IO (stdout)
import Types

main :: IO ()
main = do
    args <- getArgs
    cfg <- readConfigFromArgs args
    case cfg of
        Right cfg' -> do
            TIO.putStrLn "Type \"help\" for help"
            repl cfg'
            stdoutSupportsANSI <- hNowSupportsANSI stdout
            when stdoutSupportsANSI $ do
                setSGR [Reset]
        Left err -> printErrs err
