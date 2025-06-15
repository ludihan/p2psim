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
    loop = do
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
            Just input ->
                case parse parseCommand "<stdin>" (T.pack input) of
                    Left e -> do
                        liftIO $ printErrs [Err $ T.pack $ errorBundlePretty e]
                        loop
                    Right Help -> do
                        liftIO $ TIO.putStrLn showHelp
                        loop
                    Right List -> do
                        liftIO $ TIO.putStrLn $ showConfigList cfg
                        loop
                    Right (SearchCommand search) -> do
                        simulation cfg search
                        loop
                    Right Algo -> do
                        liftIO $ TIO.putStrLn showAvailableAlgo
                        loop
                    Right Reload -> do
                        args <- liftIO getArgs
                        cfg' <- liftIO $ readConfigFromArgs args
                        case cfg' of
                            Right cfg'' -> do
                                liftIO $ TIO.putStrLn "config file successfully read!"
                                liftIO $ repl cfg''
                            Left err -> do
                                liftIO $
                                    printErrs $
                                        Err
                                            "failed to load config file, \
                                            \using previous configuration"
                                            : err
                                loop
                    Right Render -> do
                        liftIO $ renderConfig cfg
                        loop
                    Right Quit -> return ()

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
