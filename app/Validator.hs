{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Validator (
    validateTOML,
    validateSearch,
) where

import Types

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

validateTOML :: Config -> Either [Err] Config
validateTOML cfg@Config{..} =
    let
        neighborErrs = neighborErrors minNeighbors maxNeighbors edges
        selfEdgeErrs = selfEdgeErrors edges
        emptyNodeErrs = nodesWithoutResources resources
        edgeMissingNodes = nodesMissingInNodeMap edges resources
        nodeUnusedErrs = nodesUnusedInEdges resources edges

        allErrs =
            concat
                [ neighborErrs
                , selfEdgeErrs
                , emptyNodeErrs
                , edgeMissingNodes
                , nodeUnusedErrs
                ]
     in
        if null allErrs
            then
                Right
                    cfg
                        { resources =
                            Map.fromList
                                [(k, nub v) | (k, v) <- Map.toList resources]
                        }
            else Left $ map Err allErrs

validateSearch :: Config -> Search -> [Err]
validateSearch Config{..} Search{..} =
    let errors =
            [ if nodeId `notElem` Map.keys resources
                then Just $ Err $ "node \"" <> nodeId <> "\" does not exist"
                else Nothing
            , if ttl < 0
                then Just $ Err "must be non-negative"
                else Nothing
            , if not (resourceExists resourceId resources)
                then Just $ Err $ "resource \"" <> resourceId <> "\" not found in any node"
                else Nothing
            ]
     in catMaybes errors

resourceExists :: Text -> Resources -> Bool
resourceExists rid = any (elem rid)

neighborErrors :: Maybe Int -> Maybe Int -> Edges -> [Text]
neighborErrors minN maxN edges =
    let nodeMap = buildAdjacencyFromEdges edges
     in concatMap (checkNode minN maxN) (Map.toList nodeMap)

checkNode :: Maybe Int -> Maybe Int -> (Text, [Text]) -> [Text]
checkNode minN maxN (node, neighbors) =
    catMaybes
        [ case minN of
            Just n
                | length neighbors < n ->
                    Just $ "node \"" <> node <> "\" has fewer than " <> T.pack (show n) <> " neighbors"
            _ -> Nothing
        , case maxN of
            Just n
                | length neighbors > n ->
                    Just $ "node \"" <> node <> "\" has more than " <> T.pack (show n) <> " neighbors"
            _ -> Nothing
        ]

selfEdgeErrors :: Edges -> [Text]
selfEdgeErrors edges =
    [ "self-edge detected at node \"" <> a <> "\""
    | [a, b] <- edges
    , a == b
    ]

nodesWithoutResources :: Resources -> [Text]
nodesWithoutResources nodeMap =
    [ "node \"" <> node <> "\" has no resources"
    | (node, resources) <- Map.toList nodeMap
    , null resources
    ]

nodesMissingInNodeMap :: Edges -> Resources -> [Text]
nodesMissingInNodeMap edges nodesMap =
    let
        edgeNodes = [n | [a, b] <- edges, n <- [a, b]]
        nodeSet = Map.keysSet nodesMap
     in
        [ "node \"" <> n <> "\" is declared in \"edges\" but has no resources"
        | n <- nub edgeNodes
        , n `notElem` nodeSet
        ]

nodesUnusedInEdges :: Resources -> Edges -> [Text]
nodesUnusedInEdges nodesMap edges =
    let
        edgeNodes = [n | [a, b] <- edges, n <- [a, b]]
        edgeNodeSet = nub edgeNodes
     in
        [ "node \"" <> n <> "\" is declared in \"resources\" but has no edges."
        | n <- Map.keys nodesMap
        , n `notElem` edgeNodeSet
        ]
