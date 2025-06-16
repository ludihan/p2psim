{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config where

import Data.GraphViz
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import TOML
import Types
import Validator

readConfigFromArgs :: [String] -> IO (Either [Err] Config)
readConfigFromArgs [] = return (Left [Err "no config file"])
readConfigFromArgs (x : _) = readConfig x

readConfig :: FilePath -> IO (Either [Err] Config)
readConfig cfgPath = do
    file <- decodeFile cfgPath
    return $ case file of
        Right cfg -> validateTOML cfg
        Left err -> Left [Err $ renderTOMLError err]

renderConfig :: Config -> IO ()
renderConfig Config{..} = do
    let adj = convertAdjMap $ buildAdjacencyFromEdges edges
    let myGraph = fromAdjacencyMap adj resources
    fPath <- runGraphvizCommand Circo myGraph Png "graph.png"
    putStrLn $ "graph written to " ++ fPath

fromAdjacencyMap :: Map.Map TL.Text [TL.Text] -> Resources -> Data.GraphViz.DotGraph TL.Text
fromAdjacencyMap adj res =
    Data.GraphViz.DotGraph
        { strictGraph = False
        , directedGraph = False
        , graphID = Nothing
        , graphStatements =
            DotStmts
                { attrStmts = []
                , subGraphs = []
                , nodeStmts =
                    [ DotNode
                        (TL.fromStrict k)
                        [ shape Circle
                        , textLabel (TL.fromStrict k <> "\n" <> TL.fromStrict (fmtList v))
                        ]
                    | (k, v) <- Map.toList res
                    ]
                , edgeStmts = [DotEdge a b [] | (a, bs) <- Map.toList adj, b <- bs, a < b]
                }
        }

convertAdjMap :: Map.Map T.Text [T.Text] -> Map.Map TL.Text [TL.Text]
convertAdjMap = Map.mapKeys TL.fromStrict . fmap (map TL.fromStrict)
