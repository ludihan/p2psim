{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Repl (
    repl,
) where

import Config
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Simulator
import System.Console.ANSI
import System.Console.Haskeline
import System.Environment
import System.IO (stdout)
import Text.Megaparsec
import Types

repl :: Config -> IO ()
repl cfg =
    runInputT
        Settings
            { historyFile = Nothing
            , complete = noCompletion
            , autoAddHistory = True
            }
        loop
  where
    loop :: InputT IO ()
    loop = handleInterrupt loop $ withInterrupt $ do
        stdoutSupportsANSI <- liftIO $ hNowSupportsANSI stdout
        let haskeline =
                if stdoutSupportsANSI
                    then do
                        liftIO $ setSGR [SetConsoleIntensity BoldIntensity]
                        liftIO $ setSGR [SetColor Foreground Dull Green]
                        minput' <- getInputLine "p2psim% "
                        liftIO $ setSGR [Reset]
                        return minput'
                    else getInputLine "p2psim% "
        minput <- haskeline
        case minput of
            Nothing -> return ()
            Just input -> do
                result <- liftIO $ handleCommand cfg input
                case result of
                    Left () -> return ()
                    Right Nothing -> loop
                    Right (Just a) -> liftIO $ repl a

handleCommand :: Config -> String -> IO (Either () (Maybe Config))
handleCommand cfg input =
    case parse parseCommand "<stdin>" (T.pack input) of
        Left e -> do
            printErrs [Err $ T.pack $ errorBundlePretty e]
            return $ Right Nothing
        Right Help -> do
            TIO.putStrLn showHelp
            return $ Right Nothing
        Right List -> do
            TIO.putStrLn $ showConfigList cfg
            return $ Right Nothing
        Right (SearchCommand search) -> do
            simulation cfg search
            return $ Right Nothing
        Right Algo -> do
            TIO.putStrLn showAvailableAlgo
            return $ Right Nothing
        Right Reload -> do
            args <- getArgs
            cfg' <- readConfigFromArgs args
            case cfg' of
                Right newCfg -> do
                    TIO.putStrLn "config file successfully reloaded!"
                    return $ Right (Just newCfg)
                Left err -> do
                    printErrs $
                        Err "failed to reload config file, using previous configuration"
                            : err
                    return $ Right Nothing
        Right Render -> do
            result <- renderConfig cfg
            case result of
                Right _ ->
                    return ()
                Left err ->
                    printErr err
            return $ Right Nothing
        Right Quit -> return $ Left ()

showHelp :: Text
showHelp =
    "use \"help\" for help\n\
    \use \"list\" to list all settings from the config file\n\
    \use \"render\" to render the current graph to \"graph.png\"\n\
    \use \"search\" with the nodeID, resourceID, ttl and a algorithm to search for a resource\n\
    \use \"algo\" to list all available algorithms\n\
    \use \"reload\" to reload the config file\n\
    \use \"quit\" to quit the program"

showAvailableAlgo :: Text
showAvailableAlgo =
    T.intercalate
        "\n"
        [ "available algorithms are:"
        , "  flooding"
        , "  random_walk"
        ]

showConfigList :: Config -> Text
showConfigList Config{..} =
    T.concat
        [ "max_neighbours:\n"
        , maybe "not set" (T.pack . show) maxNeighbors
        , "\n"
        , "\n"
        , "min_neighbours:\n"
        , maybe "not set" (T.pack . show) minNeighbors
        , "\n"
        , "\n"
        , "resources:\n"
        , fmtResources resources
        , "\n"
        , "\n"
        , "edges:\n"
        , fmtEdges edges
        ]
