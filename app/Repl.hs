{-# LANGUAGE RecordWildCards #-}

module Repl (
    repl,
) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import Simulator
import System.Console.Haskeline
import Text.Megaparsec (parse)
import Text.Megaparsec.Error
import Types

repl :: Config -> IO ()
repl cfg@Config{..} = runInputT defaultSettings loop
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
                        liftIO $ printErrs [Err $ errorBundlePretty e]
                        loop
                    (Right a) -> do
                        case a of
                            (SearchCommand search) -> do
                                simulate cfg search
                                loop
                            Help -> do
                                outputStrLn $ show a
                                loop
                            List -> do
                                outputStrLn $
                                    "max_neighbours:\n"
                                        ++ maybe "not set" show maxNeighbors
                                        ++ "\n"
                                outputStrLn $
                                    "min_neighbours:\n"
                                        ++ maybe "not set" show minNeighbors
                                        ++ "\n"
                                outputStrLn $
                                    "resources:\n"
                                        ++ fmtResources resources
                                outputStrLn $
                                    "edges:\n"
                                        ++ fmtEdges edges
                                loop
fmtEdges :: Edges -> String
fmtEdges edges =
    let adj = buildAdjacencyFromEdges edges
     in fmtList $ Map.toList adj

fmtResources :: Resources -> String
fmtResources res =
    fmtList $ Map.toList res

fmtList :: (Show k, Show v) => [(k, v)] -> String
fmtList list = unlines [show k ++ " => " ++ show v | (k, v) <- list]
