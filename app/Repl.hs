module Repl (
    repl,
) where

import Simulator
import System.Console.Haskeline
import Text.Megaparsec (parse)
import Text.Megaparsec.Error
import Types

repl :: Config -> IO ()
repl cfg = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
        minput <- getInputLine "p2psim% "
        case minput of
            Nothing -> return ()
            Just "quit" -> return ()
            Just input ->
                case parse parseCommand "<stdin>" input of
                    (Left e) -> do
                        outputStr $ errorBundlePretty e
                        loop
                    (Right a) -> do
                        case a of
                            (SearchCommand search) -> do
                                simulate cfg search
                                loop
                            Help -> do
                                outputStrLn $ show a
                                loop
