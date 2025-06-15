{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Simulator (
    simulation,
) where

import Control.Monad.IO.Class (liftIO)
import Data.List (sort)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.Console.Haskeline
import System.Random
import Types
import Validator

simulation :: Config -> Search -> InputT IO ()
simulation config search@Search{..} = do
    isValid <- validateSearch config search
    if not isValid
        then liftIO $ TIO.putStrLn "Validation failed. Simulation aborted."
        else do
            gen <- liftIO newStdGen
            let strategy = strategyFor algo
                (logTrace, result) = graphSearch config search strategy gen
            mapM_ (liftIO . TIO.putStrLn . showSearch) logTrace
            liftIO $ TIO.putStrLn (if result then "Search successful!" else "Search failed...")

graphSearch ::
    Config ->
    Search ->
    SearchStrategy ->
    StdGen ->
    SearchResult
graphSearch config@Config{..} search@Search{..} strategy@SearchStrategy{..} gen =
    go gen Nothing start ttl Set.empty
  where
    adj = buildAdjacencyFromEdges edges
    start = nodeId
    target = resourceId

    go :: StdGen -> Maybe Text -> Text -> Int -> Set.Set Text -> SearchResult
    go _ _ _ 0 _ =
        let step = Search start Nothing target 0 algo False
         in ([step], False)
    go g parent current t visited
        | hasResource current target resources =
            let step = Search current parent target t algo True
             in ([step], True)
        | otherwise =
            let visited' = Set.insert current visited
                neighbors0 = fromMaybe [] (Map.lookup current adj)
                neighbors1 = filter (`Set.notMember` visited') neighbors0
                ordered = order neighbors1
                (nextNodes, g') =
                    if useRandom
                        then selectNext ordered visited' g
                        else (ordered, g)
                step = Search current parent target t algo False
                outcomes = map (\n -> go g' (Just current) n (t - 1) visited') nextNodes
                (traces, results) = unzip outcomes
                found' = or results
             in (step : concat traces, found')

floodingStrategy :: SearchStrategy
floodingStrategy =
    SearchStrategy
        { selectNext = \ns _ g -> (ns, g)
        , order = id
        , useRandom = False
        }

informedFloodingStrategy :: SearchStrategy
informedFloodingStrategy = floodingStrategy{order = sort}

randomWalkStrategy :: SearchStrategy
randomWalkStrategy =
    SearchStrategy
        { selectNext = \ns _ g ->
            let (i, g') = randomR (0, length ns - 1) g
             in ([ns !! i], g')
        , order = id
        , useRandom = True
        }

informedRandomWalkStrategy :: SearchStrategy
informedRandomWalkStrategy = randomWalkStrategy{order = sort}

strategyFor :: SearchAlgorithm -> SearchStrategy
strategyFor Flooding = floodingStrategy
strategyFor InformedFlooding = informedFloodingStrategy
strategyFor RandomWalk = randomWalkStrategy
strategyFor InformedRandomWalk = informedRandomWalkStrategy

hasResource :: Text -> Text -> Resources -> Bool
hasResource nCurrent res nodesMap = maybe False (elem res) (Map.lookup nCurrent nodesMap)
