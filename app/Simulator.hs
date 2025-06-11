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
import Types
import Validator

hasResource :: String -> String -> Resources -> Bool
hasResource node res nodesMap = maybe False (elem res) (Map.lookup node nodesMap)

simulate :: Config -> Search -> InputT IO ()
simulate config@Config{..} search@Search{..} = do
    isValid <- validateSearch config search
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

    go _ _ 0 _ = return SearchNotFound
    go mParent node t visited
        | hasResource node target resources = do
            outputStrLn $ "Found resource at " ++ node
            return SearchFound
        | otherwise = do
            let visited' = Set.insert node visited
            let neighbors = fromMaybe [] (Map.lookup node adj)
            let nextNodes = filter (`Set.notMember` visited') neighbors
            outputStrLn $
                case mParent of
                    Just p ->
                        "flooding: " ++ p ++ " -> " ++ node ++ " (TTL=" ++ show t ++ ")"
                    Nothing ->
                        "flooding: " ++ node ++ " -> " ++ node ++ " (TTL=" ++ show t ++ ")"
            results <- mapM (\n -> go (Just node) n (t - 1) visited') nextNodes
            return $ if SearchFound `elem` results then SearchFound else SearchNotFound

informedFlooding :: Resources -> Edges -> String -> String -> Int -> InputT IO SearchResult
informedFlooding resources edges start target ttl = go Nothing start ttl Set.empty
  where
    adj = buildAdjacencyFromEdges edges

    go _ _ 0 _ = return SearchNotFound
    go mParent node t visited
        | hasResource node target resources = do
            outputStrLn $ "Found resource at " ++ node
            return SearchFound
        | otherwise = do
            let visited' = Set.insert node visited
            let neighbors = fromMaybe [] (Map.lookup node adj)
            let nextNodes = filter (`Set.notMember` visited') $ sort neighbors
            outputStrLn $
                case mParent of
                    Just p ->
                        "informed_flooding: " ++ p ++ " -> " ++ node ++ " (TTL=" ++ show t ++ ")"
                    Nothing ->
                        "informed_flooding: " ++ node ++ " -> " ++ node ++ " (TTL=" ++ show t ++ ")"
            results <- mapM (\n -> go (Just node) n (t - 1) visited') nextNodes
            return $ if SearchFound `elem` results then SearchFound else SearchNotFound

randomWalk :: Resources -> Edges -> String -> String -> Int -> InputT IO SearchResult
randomWalk resources edges start target ttl = go start ttl Set.empty
  where
    adj = buildAdjacencyFromEdges edges

    go _ 0 _ = return SearchNotFound
    go node t visited
        | hasResource node target resources = do
            outputStrLn $ "Found resource at " ++ node
            return SearchFound
        | otherwise = do
            let visited' = Set.insert node visited
            let neighbors = filter (`Set.notMember` visited') (fromMaybe [] (Map.lookup node adj))
            if null neighbors
                then return SearchNotFound
                else do
                    i <- liftIO $ randomRIO (0, length neighbors - 1)
                    let next' = neighbors !! i
                    outputStrLn $ "random_walk: " ++ node ++ " -> " ++ next' ++ " (TTL=" ++ show t ++ ")"
                    go next' (t - 1) visited'

informedRandomWalk :: Resources -> Edges -> String -> String -> Int -> InputT IO SearchResult
informedRandomWalk resources edges start target ttl = go start ttl Set.empty
  where
    adj = buildAdjacencyFromEdges edges

    go _ 0 _ = return SearchNotFound
    go node t visited
        | hasResource node target resources = do
            outputStrLn $ "Found resource at " ++ node
            return SearchFound
        | otherwise = do
            let visited' = Set.insert node visited
            let neighbors = sort $ filter (`Set.notMember` visited') (fromMaybe [] (Map.lookup node adj))
            if null neighbors
                then return SearchNotFound
                else do
                    i <- liftIO $ randomRIO (0, length neighbors - 1)
                    let next' = neighbors !! i
                    outputStrLn $ "informed_random_walk " ++ node ++ " -> " ++ next' ++ " (TTL=" ++ show t ++ ")"
                    go next' (t - 1) visited'
