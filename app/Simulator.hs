{-# LANGUAGE RecordWildCards #-}

module Simulator (
    simulate,
) where

import Control.Monad.IO.Class (liftIO)
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import System.Console.Haskeline
import System.Random
import Text.Printf
import Types
import Validator

hasResource :: String -> String -> Resources -> Bool
hasResource nCurrent res nodesMap = maybe False (elem res) (Map.lookup nCurrent nodesMap)

simulate :: Config -> Search -> InputT IO ()
simulate config@Config{..} search@Search{..} = do
    isValid <- validateSearch config search
    if not isValid
        then outputStrLn "Validation failed. Simulation aborted."
        else do
            outputStrLn $
                intercalate
                    "\n"
                    [ "Search request : "
                    , "  From node    : " ++ nodeId
                    , "  For resource : " ++ resourceId
                    , "  TTL          : " ++ show ttl
                    , "  Algorithm    : " ++ show algo
                    , ""
                    ]
            result <- case algo of
                Flooding -> flooding resources edges nodeId resourceId ttl
                InformedFlooding -> informedFlooding resources edges nodeId resourceId ttl
                RandomWalk -> randomWalk resources edges nodeId resourceId ttl
                InformedRandomWalk -> informedRandomWalk resources edges nodeId resourceId ttl

            outputStrLn $ case result of
                SearchFound -> "Search successful!\n"
                SearchNotFound -> "Search failed...\n"

flooding :: Resources -> Edges -> String -> String -> Int -> InputT IO SearchResult
flooding resources edges start target ttl = go Nothing start ttl Set.empty
  where
    adj = buildAdjacencyFromEdges edges

    go nParent nCurrent t@0 _ = do
        outputStrLn $ showRequest Flooding nParent nCurrent t False
        return SearchNotFound
    go nParent nCurrent t visited
        | hasResource nCurrent target resources = do
            outputStrLn $ showRequest Flooding nParent nCurrent t True
            return SearchFound
        | otherwise = do
            let visited' = Set.insert nCurrent visited
            let neighbors = fromMaybe [] (Map.lookup nCurrent adj)
            let nextNodes = filter (`Set.notMember` visited') neighbors
            outputStrLn $ showRequest Flooding nParent nCurrent t False
            results <- mapM (\n -> go (Just nCurrent) n (t - 1) visited') nextNodes
            return $ if SearchFound `elem` results then SearchFound else SearchNotFound

informedFlooding :: Resources -> Edges -> String -> String -> Int -> InputT IO SearchResult
informedFlooding resources edges start target ttl = go Nothing start ttl Set.empty
  where
    adj = buildAdjacencyFromEdges edges

    go nParent nCurrent t@0 _ = do
        outputStrLn $ showRequest InformedFlooding nParent nCurrent t False
        return SearchNotFound
    go nParent nCurrent t visited
        | hasResource nCurrent target resources = do
            outputStrLn $ showRequest InformedFlooding nParent nCurrent t True
            return SearchFound
        | otherwise = do
            let visited' = Set.insert nCurrent visited
            let neighbors = fromMaybe [] (Map.lookup nCurrent adj)
            let nextNodes = filter (`Set.notMember` visited') $ sort neighbors
            outputStrLn $ showRequest InformedFlooding nParent nCurrent t False
            results <- mapM (\n -> go (Just nCurrent) n (t - 1) visited') nextNodes
            return $ if SearchFound `elem` results then SearchFound else SearchNotFound

randomWalk :: Resources -> Edges -> String -> String -> Int -> InputT IO SearchResult
randomWalk resources edges start target ttl = go Nothing start ttl Set.empty
  where
    adj = buildAdjacencyFromEdges edges

    go nParent nCurrent t@0 _ = do
        outputStrLn $ showRequest RandomWalk nParent nCurrent t False
        return SearchNotFound
    go nParent nCurrent t visited
        | hasResource nCurrent target resources = do
            outputStrLn $ showRequest RandomWalk nParent nCurrent t True
            return SearchFound
        | otherwise = do
            let visited' = Set.insert nCurrent visited
            let neighbors = filter (`Set.notMember` visited') (fromMaybe [] (Map.lookup nCurrent adj))
            if null neighbors
                then return SearchNotFound
                else do
                    i <- liftIO $ randomRIO (0, length neighbors - 1)
                    let next' = neighbors !! i
                    outputStrLn $ showRequest RandomWalk nParent nCurrent t False
                    go (Just nCurrent) next' (t - 1) visited'

informedRandomWalk :: Resources -> Edges -> String -> String -> Int -> InputT IO SearchResult
informedRandomWalk resources edges start target ttl = go Nothing start ttl Set.empty
  where
    adj = buildAdjacencyFromEdges edges

    go nParent nCurrent t@0 _ = do
        outputStrLn $ showRequest InformedRandomWalk nParent nCurrent t False
        return SearchNotFound
    go nParent nCurrent t visited
        | hasResource nCurrent target resources = do
            outputStrLn $ showRequest InformedRandomWalk nParent nCurrent t True
            return SearchFound
        | otherwise = do
            let visited' = Set.insert nCurrent visited
            let neighbors = sort $ filter (`Set.notMember` visited') (fromMaybe [] (Map.lookup nCurrent adj))
            if null neighbors
                then return SearchNotFound
                else do
                    i <- liftIO $ randomRIO (0, length neighbors - 1)
                    let next' = neighbors !! i
                    outputStrLn $ showRequest InformedRandomWalk nParent nCurrent t False
                    go (Just nCurrent) next' (t - 1) visited'

showRequest :: SearchAlgorithm -> Maybe String -> String -> Int -> Bool -> String
showRequest algo parent current ttl found =
    printf
        "[%s] [TTL=%d]: %s -> %s (%s)"
        (show algo)
        ttl
        (fromMaybe current parent)
        current
        ( if ttl == 0
            then "dead"
            else
                if found
                    then
                        "resource found"
                    else "searching"
        )
