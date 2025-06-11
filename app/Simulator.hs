{-# LANGUAGE RecordWildCards #-}

module Simulator (
    simulate,
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import System.Console.Haskeline
import Types
import System.Random
import Data.List
import Control.Monad.IO.Class (liftIO)

type Visited = Set.Set String

-- Turn edge list into an adjacency map
buildAdjacency :: Edges -> Map String [String]
buildAdjacency = Map.fromListWith (++) . concatMap expandEdge
  where
    expandEdge [a, b] = [(a, [b]), (b, [a])]
    expandEdge _ = []

-- Check if a node has the resource
hasResource :: String -> String -> Nodes -> Bool
hasResource node res nodesMap = maybe False (elem res) (Map.lookup node nodesMap)

simulate :: Config -> Search -> InputT IO ()
simulate config@Config{..} search@Search{..} = do
    outputStrLn $ show config
    outputStrLn $ show search
    isValid <- validate config search
    if not isValid
        then outputStrLn "Validation failed. Simulation aborted."
        else do
            outputStrLn "Search request:"
            outputStrLn $ "  From node:    " ++ nodeId
            outputStrLn $ "  For resource: " ++ resourceId
            outputStrLn $ "  TTL:          " ++ show ttl
            outputStrLn $ "  Algorithm:    " ++ show algo
            outputStrLn ""

            result <- case algo of
                Flooding -> flooding nodes edges nodeId resourceId ttl
                InformedFlooding -> informedFlooding nodes edges nodeId resourceId ttl
                RandomWalk -> randomWalk nodes edges nodeId resourceId ttl
                InformedRandomWalk -> informedRandomWalk nodes edges nodeId resourceId ttl
            outputStrLn $ case result of
                SearchFound -> "Search successful!"
                SearchNotFound -> "Search failed: resource not found."

validate :: Config -> Search -> InputT IO Bool
validate Config{..} Search{..} = do
    let errors =
            [ if nodeId `notElem` Map.keys nodes
                then Just $ "Error: node \"" ++ nodeId ++ "\" does not exist."
                else Nothing
            , if ttl < 0
                then Just "Error: TTL must be non-negative."
                else Nothing
            , if not (resourceExists resourceId nodes)
                then Just $ "Error: resource \"" ++ resourceId ++ "\" not found in any node."
                else Nothing
            ]
                ++ neighborErrors minNeighbors maxNeighbors nodes

    let errs = catMaybes errors

    mapM_ outputStrLn errs
    return $ null errs -- success = no errors

-- Check if the resource exists in any node
resourceExists :: String -> Nodes -> Bool
resourceExists rid = any (elem rid)

-- Check neighbor counts of all nodes
neighborErrors :: Maybe Int -> Maybe Int -> Nodes -> [Maybe String]
neighborErrors minN maxN nodeMap =
    let check (node, neighbors) =
            let count = length neighbors
                minErr = case minN of
                    Just n | count < n -> Just $ "Error: node \"" ++ node ++ "\" has fewer than " ++ show n ++ " neighbors."
                    _ -> Nothing
                maxErr = case maxN of
                    Just n | count > n -> Just $ "Error: node \"" ++ node ++ "\" has more than " ++ show n ++ " neighbors."
                    _ -> Nothing
             in [minErr, maxErr]
     in concatMap check (Map.toList nodeMap)

flooding :: Nodes -> Edges -> String -> String -> Int -> InputT IO SearchResult
flooding nodes edges start target ttl = go start ttl Set.empty
  where
    adj = buildAdjacency edges

    go node 0 _ = return SearchNotFound
    go node t visited
        | hasResource node target nodes = do
            outputStrLn $ "Found resource at " ++ node
            return SearchFound
        | otherwise = do
            let visited' = Set.insert node visited
            let neighbors = fromMaybe [] (Map.lookup node adj)
            let next = filter (`Set.notMember` visited') neighbors
            outputStrLn $ "Visiting " ++ node ++ " (TTL=" ++ show t ++ ")"
            searchResults <- mapM (\n -> go n (t - 1) visited') next
            return $ if SearchFound `elem` searchResults then SearchFound else SearchNotFound

informedFlooding :: Nodes -> Edges -> String -> String -> Int -> InputT IO SearchResult
informedFlooding nodes edges start target ttl = go start ttl Set.empty
  where
    adj = buildAdjacency edges

    go node 0 _ = return SearchNotFound
    go node t visited
        | hasResource node target nodes = do
            outputStrLn $ "Found resource at " ++ node
            return SearchFound
        | otherwise = do
            let visited' = Set.insert node visited
            let neighbors = fromMaybe [] (Map.lookup node adj)
            let sortedNeighbors = filter (`Set.notMember` visited') (sort neighbors)
            outputStrLn $ "Visiting " ++ node ++ " (TTL=" ++ show t ++ ")"
            searchResults <- mapM (\n -> go n (t - 1) visited') sortedNeighbors
            return $ if SearchFound `elem` searchResults then SearchFound else SearchNotFound

randomWalk :: Nodes -> Edges -> String -> String -> Int -> InputT IO SearchResult
randomWalk nodes edges start target ttl = go start ttl Set.empty
  where
    adj = buildAdjacency edges

    go node 0 _ = return SearchNotFound
    go node t visited
        | hasResource node target nodes = do
            outputStrLn $ "Found resource at " ++ node
            return SearchFound
        | otherwise = do
            let visited' = Set.insert node visited
            let neighbors = filter (`Set.notMember` visited') (fromMaybe [] (Map.lookup node adj))
            if null neighbors
                then return SearchNotFound
                else do
                    i <- liftIO $ randomRIO (0, length neighbors - 1)
                    let next = neighbors !! i
                    outputStrLn $ "Randomly walking from " ++ node ++ " to " ++ next ++ " (TTL=" ++ show t ++ ")"
                    go next (t - 1) visited'

informedRandomWalk :: Nodes -> Edges -> String -> String -> Int -> InputT IO SearchResult
informedRandomWalk nodes edges start target ttl = go start ttl Set.empty
  where
    adj = buildAdjacency edges

    go node 0 _ = return SearchNotFound
    go node t visited
        | hasResource node target nodes = do
            outputStrLn $ "Found resource at " ++ node
            return SearchFound
        | otherwise = do
            let visited' = Set.insert node visited
            let neighbors = sort $ filter (`Set.notMember` visited') (fromMaybe [] (Map.lookup node adj))
            if null neighbors
                then return SearchNotFound
                else do
                    i <- liftIO $ randomRIO (0, length neighbors - 1)
                    let next = neighbors !! i
                    outputStrLn $ "Informed walk from " ++ node ++ " to " ++ next ++ " (TTL=" ++ show t ++ ")"
                    go next (t - 1) visited'
