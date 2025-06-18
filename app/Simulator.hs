{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Simulator (
    simulation,
) where

import Data.List
import qualified Data.Map as Map
import Data.Ord
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
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
            let nodeCounts = countNodeActivity trace
            let (node, times) = head $ sortOn (Down . snd) (Map.toList nodeCounts)
            mapM_ (TIO.putStrLn . showSearch) (tail trace)
            TIO.putStrLn $ "total of " <> T.pack (show $ length (tail trace)) <> " messages."
            TIO.putStrLn $
                T.concat
                    [ "most messages mentioned node "
                    , node
                    , " ("
                    , T.pack $ show times
                    , " times"
                    , ")"
                    ]
            TIO.putStrLn (if any found trace then "search successful!" else "search failed...")

countNodeActivity :: [Search] -> Map.Map Text Int
countNodeActivity = foldr add Map.empty
  where
    add Search{..} acc =
        let acc' = Map.insertWith (+) nodeId 1 acc
         in case nodeParentId of
                Just pid -> Map.insertWith (+) pid 1 acc'
                Nothing -> acc'

graphSearch :: Config -> Search -> StdGen -> SearchResults
graphSearch Config{..} search gen
    | Set.member (nodeId search) (visited search) =
        [search{found = False}]
    | hasResource (nodeId search) (resourceId search) resources =
        [search{found = True}]
    | ttl search <= 0 =
        [search{found = False}]
    | otherwise =
        let
            adj = buildAdjacencyFromEdges edges
            neighbors = Map.findWithDefault [] (nodeId search) adj

            visited' = Set.insert (nodeId search) (visited search)

            unvisited = filter (`Set.notMember` visited') neighbors

            sortByDegreeDesc :: [Text] -> [Text]
            sortByDegreeDesc =
                let degree n = length (Map.findWithDefault [] n adj)
                 in sortOn (Data.Ord.Down . degree)

            weightedRandom :: [a] -> [Int] -> StdGen -> (a, StdGen)
            weightedRandom xs ws gen'' =
                let total = sum ws
                    (r, gen''') = randomR (1, total) gen''
                 in (pick r (zip xs (scanl1 (+) ws)), gen''')
              where
                pick r ((x, w) : rest)
                    | r <= w = x
                    | otherwise = pick r rest
                pick _ [] = error "weightedRandom: empty list"

            (nextNodes, gen') =
                case algo search of
                    Flooding -> (unvisited, gen)
                    InformedFlooding -> (sortByDegreeDesc unvisited, gen)
                    RandomWalk ->
                        if null unvisited
                            then ([], gen)
                            else
                                let (idx, g') = randomR (0, length unvisited - 1) gen
                                 in ([unvisited !! idx], g')
                    InformedRandomWalk ->
                        if null unvisited
                            then ([], gen)
                            else
                                let degrees = map (\n -> (n, length $ Map.findWithDefault [] n adj)) unvisited
                                    (choices, weights) = unzip degrees
                                    (choice, g') = weightedRandom choices weights gen
                                 in ([choice], g')

            nextSearches =
                [ Search
                    { nodeId = nid
                    , nodeParentId = Just (nodeId search)
                    , resourceId = resourceId search
                    , ttl = ttl search - 1
                    , algo = algo search
                    , found = False
                    , visited = visited'
                    }
                | nid <- nextNodes
                ]

            nextResults =
                concatMap (\s -> graphSearch Config{..} s gen') nextSearches
         in
            search{visited = visited'} : nextResults

hasResource :: Text -> Text -> Resources -> Bool
hasResource nCurrent res nodesMap = maybe False (elem res) (Map.lookup nCurrent nodesMap)
