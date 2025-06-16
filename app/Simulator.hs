{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Simulator (
    simulation,
) where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.Random
import Types
import Validator

simulation :: Config -> Search -> IO ()
simulation config search = do
    let errs = validateSearch config search
    if not (null errs)
        then printErrs errs
        else do
            gen <- newStdGen
            let trace = graphSearch config search gen
            mapM_ (TIO.putStrLn . showSearch) trace
            TIO.putStrLn (if any found trace then "search successful!" else "search failed...")

graphSearch ::
    Config ->
    Search ->
    StdGen ->
    SearchResults
graphSearch config@Config{..} search@Search{..} gen
    | Set.member nodeId (Set.map head visited) = [search{found = False}]
    | hasResource nodeId resourceId resources = [search{found = True}]
    | ttl <= 0 = [search{found = False}]
    | otherwise =
        let adj = buildAdjacencyFromEdges edges
            visited' = Set.insert [nodeId] visited
            neighbors = fromMaybe [] (Map.lookup nodeId adj)
            unvisited = filter (\n -> not (Set.member [n] visited')) neighbors
         in case unvisited of
                [] -> [search{visited = visited', found = False}]
                _ ->
                    let (nextNodes, gen') = case algo of
                            Flooding -> (unvisited, gen)
                            InformedFlooding -> (unvisited, gen)
                            RandomWalk ->
                                let (idx, g') = randomR (0, length unvisited - 1) gen
                                 in ([unvisited !! idx], g')
                            InformedRandomWalk ->
                                let (idx, g') = randomR (0, length unvisited - 1) gen
                                 in ([unvisited !! idx], g')

                        nextSearches =
                            map
                                ( \nextNode ->
                                    Search
                                        { nodeId = nextNode
                                        , nodeParentId = Just nodeId
                                        , resourceId = resourceId
                                        , ttl = ttl - 1
                                        , algo = algo
                                        , found = False
                                        , visited = visited'
                                        , cache = cache
                                        }
                                )
                                nextNodes
                        traces = concatMap (\s -> graphSearch config s gen') nextSearches
                     in search : traces

hasResource :: Text -> Text -> Resources -> Bool
hasResource nCurrent res nodesMap = maybe False (elem res) (Map.lookup nCurrent nodesMap)
