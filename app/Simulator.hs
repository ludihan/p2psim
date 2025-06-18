{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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
            mapM_ (TIO.putStrLn . showSearch) (tail trace)
            TIO.putStrLn $ "total of " <> T.pack (show $ length (tail trace)) <> " messages."
            if length trace == 1
                then
                    TIO.putStrLn $
                        T.concat
                            [ "node "
                            , nodeId search
                            , " already has the resource"
                            ]
                else do
                    let nodeCounts = countNodeActivity (tail trace)
                    let (node, times) = head $ sortOn (Down . snd) (Map.toList nodeCounts)
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

            shuffle :: StdGen -> [a] -> ([a], StdGen)
            shuffle gen''' xs = shuffle' gen''' xs []
              where
                shuffle' g [] acc = (acc, g)
                shuffle' g ys acc =
                    let (i, g') = randomR (0, length ys - 1) g
                        (before, y : after) = splitAt i ys
                     in shuffle' g' (before ++ after) (y : acc)

            (nextNodes, gen') =
                case algo search of
                    Flooding -> (unvisited, gen)
                    RandomWalk ->
                        if null unvisited
                            then ([], gen)
                            else shuffle gen unvisited

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

            takeUntilFound :: [Search] -> [Search]
            takeUntilFound = go
              where
                go [] = []
                go (s : ss)
                    | found s = [s]
                    | otherwise = s : go ss
         in
            case algo search of
                RandomWalk -> takeUntilFound $ search{visited = visited'} : nextResults
                _ -> search{visited = visited'} : nextResults

hasResource :: Text -> Text -> Resources -> Bool
hasResource nCurrent res nodesMap = maybe False (elem res) (Map.lookup nCurrent nodesMap)
