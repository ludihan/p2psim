{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Map (Map)
import qualified Data.Text as T
import TOML (DecodeTOML, getField, getFieldOpt, tomlDecoder)

newtype Err = Err T.Text

data Config = Config
    { minNeighbors :: Maybe Int
    , maxNeighbors :: Maybe Int
    , nodes :: Nodes
    , edges :: Edges
    }
    deriving (Show)

type Edges = [[String]]
type Nodes = Map String [String]

instance DecodeTOML Config where
    tomlDecoder =
        Config
            <$> getFieldOpt "min_neighbours"
            <*> getFieldOpt "max_neighbours"
            <*> getField "nodes"
            <*> getField "edges"

newtype Resource = Resource String
newtype Edge = Edge [String]

data SearchParameters = SearchParameters
    { nodeId :: Int
    , resourceId :: Int
    , ttl :: Int
    , algo :: SearchAlgorithm
    }

data SearchAlgorithm
    = Flooding
    | InformedFlooding
    | RandomWalk
    | InformedRandomWalk

data SearchResponce = SearchSuccess
    { id :: Int
    }

instance Show SearchAlgorithm where
    show Flooding = "flooding"
    show InformedFlooding = "informed_flooding"
    show RandomWalk = "random_walk"
    show InformedRandomWalk = "informed_random_walk"
