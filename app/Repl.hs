{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Repl (
    repl,
) where

import Config
import Control.Monad.IO.Class
import qualified Data.Map as Map
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
repl cfg = runInputT defaultSettings loop
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
                    Nothing -> loop
                    Just a -> liftIO $ repl a

handleCommand :: Config -> String -> IO (Maybe Config)
handleCommand cfg input =
    case parse parseCommand "<stdin>" (T.pack input) of
        Left e -> do
            printErrs [Err $ T.pack $ errorBundlePretty e]
            return Nothing
        Right Help -> do
            TIO.putStrLn showHelp
            return Nothing
        Right List -> do
            TIO.putStrLn $ showConfigList cfg
            return Nothing
        Right (SearchCommand search) -> do
            simulation cfg search
            return Nothing
        Right Algo -> do
            TIO.putStrLn showAvailableAlgo
            return Nothing
        Right Reload -> do
            args <- getArgs
            cfg' <- readConfigFromArgs args
            case cfg' of
                Right newCfg -> do
                    TIO.putStrLn "config file successfully read!"
                    return (Just newCfg)
                Left err -> do
                    printErrs $
                        Err "failed to load config file, using previous configuration"
                            : err
                    return Nothing
        Right Render -> do
            renderConfig cfg
            return Nothing
        Right Quit -> return Nothing

showHelp :: Text
showHelp =
    "Use \"help\" for help\n\
    \Use \"list\" to list all settings from the config file\n\
    \Use \"render\" to render the current graph to \"graph.png\"\n\
    \Use \"search\" with the nodeID, resourceID, ttl and a algorithm to search for a resource\n\
    \Use \"algo\" to list all available algorithms\n\
    \Use \"reload\" to reload the config file\n\
    \Use \"quit\" to quit the program"

showAvailableAlgo :: Text
showAvailableAlgo =
    T.intercalate
        "\n"
        [ "Available algorithms are:"
        , "flooding"
        , "informed_flooding"
        , "random_walk"
        , "informed_random_walk"
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

fmtEdges :: Edges -> Text
fmtEdges edges =
    let adj = buildAdjacencyFromEdges edges
     in fmtList $ Map.toList adj

fmtResources :: Resources -> Text
fmtResources res =
    fmtList $ Map.toList res

fmtList :: (Show k, Show v) => [(k, v)] -> Text
fmtList list =
    T.intercalate
        "\n"
        [T.pack (show k) <> " => " <> T.pack (show v) | (k, v) <- list]
