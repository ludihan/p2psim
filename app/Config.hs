{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config where

import Data.GraphViz
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
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
    let myGraph = fromAdjacencyMap adj
    TLIO.writeFile "graph.dot" (printDotGraph myGraph)
    _ <- runGraphviz myGraph Png "graph.png"
    putStrLn "Graph written to graph.png"

fromAdjacencyMap :: Map.Map TL.Text [TL.Text] -> Data.GraphViz.DotGraph TL.Text
fromAdjacencyMap adj =
    Data.GraphViz.DotGraph
        { strictGraph = False
        , directedGraph = False
        , graphID = Just (Str "MyGraph")
        , graphStatements =
            DotStmts
                { attrStmts = []
                , subGraphs = []
                , nodeStmts = map (\n -> DotNode n [shape Circle]) nodeSet
                , edgeStmts = [DotEdge a b [] | (a, bs) <- Map.toList adj, b <- bs, a < b]
                }
        }
  where
    nodeSet = Set.toList $ Set.fromList (Map.keys adj ++ concat (Map.elems adj))

convertAdjMap :: Map.Map T.Text [T.Text] -> Map.Map TL.Text [TL.Text]
convertAdjMap = Map.mapKeys TL.fromStrict . fmap (map TL.fromStrict)
