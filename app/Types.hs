{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types where

import Control.Monad (forM_)
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Void
import System.Console.ANSI
import System.IO (stdout)
import TOML (DecodeTOML, getField, getFieldOpt, tomlDecoder)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

newtype Err = Err String

data Config = Config
    { minNeighbors :: Maybe Int
    , maxNeighbors :: Maybe Int
    , resources :: Resources
    , edges :: Edges
    }
    deriving (Show)

type Edges = [[String]]

buildAdjacencyFromEdges :: Edges -> Map.Map String [String]
buildAdjacencyFromEdges edges =
    let
        expandEdge [a, b] = [(a, [b]), (b, [a])]
        expandEdge _ = []

        allPairs = concatMap expandEdge edges

        merged = Map.fromListWith (++) allPairs
     in
        Map.map nub merged
type Resources = Map String [String]

instance DecodeTOML Config where
    tomlDecoder =
        Config
            <$> getFieldOpt "min_neighbours"
            <*> getFieldOpt "max_neighbours"
            <*> getField "resources"
            <*> getField "edges"

newtype Edge = Edge [String]
    deriving (Show)

data SearchResult = SearchFound | SearchNotFound
    deriving (Eq, Show)

data SearchAlgorithm
    = Flooding
    | InformedFlooding
    | RandomWalk
    | InformedRandomWalk

instance Show SearchAlgorithm where
    show Flooding = "flooding"
    show InformedFlooding = "informed_flooding"
    show RandomWalk = "random_walk"
    show InformedRandomWalk = "informed_random_walk"

printErrs :: [Err] -> IO ()
printErrs e =
    forM_ e $ \(Err x) -> do
        stdoutSupportsANSI <- hNowSupportsANSI stdout
        if stdoutSupportsANSI
            then do
                setSGR [SetConsoleIntensity BoldIntensity]
                setSGR [SetColor Foreground Dull Red]
                putStr "error: "
                setSGR [SetConsoleIntensity BoldIntensity]
                setSGR [SetColor Foreground Vivid White]
                putStrLn x
                setSGR [Reset]
            else do
                putStr "error: "
                putStrLn x

data Search = Search
    { nodeId :: String
    , resourceId :: String
    , ttl :: Int
    , algo :: SearchAlgorithm
    }
    deriving (Show)

data Command
    = SearchCommand Search
    | Help
    | List
    deriving (Show)

identifier :: Parser String
identifier = (:) <$> letterChar <*> many (alphaNumChar <|> single '_')

parseSearch :: Parser Command
parseSearch = do
    space
    _ <- string' "search"
    space1

    nodeId' <- identifier
    space1

    resourceId' <- identifier
    space1

    ttl' <- L.decimal
    space1

    algo' <-
        try
            ( string' "flooding"
                >> return Flooding
            )
            <|> try
                ( string' "informed_flooding"
                    >> return InformedFlooding
                )
            <|> try
                ( string' "random_walk"
                    >> return
                        RandomWalk
                )
            <|> try
                ( string' "informed_random_walk"
                    >> return InformedRandomWalk
                )

    return $ SearchCommand $ Search nodeId' resourceId' ttl' algo'

parseHelp :: Parser Command
parseHelp = do
    space
    _ <- string' "help"
    return Help

parseList :: Parser Command
parseList = do
    space
    _ <- string' "list"
    return List

parseCommand :: Parser Command
parseCommand =
    try parseSearch <|> try parseHelp <|> try parseList
