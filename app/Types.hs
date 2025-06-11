{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types where

import Data.Map (Map)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void
import System.Console.ANSI
import System.IO (stdout)
import TOML (DecodeTOML, getField, getFieldOpt, tomlDecoder)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

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
    deriving (Show)

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

printErr :: Err -> IO ()
printErr (Err e) = do
    TIO.putStrLn e
    stdoutSupportsANSI <- hNowSupportsANSI stdout
    TIO.putStr "p2psim: "
    if stdoutSupportsANSI
        then do
            setSGR [SetConsoleIntensity BoldIntensity]
            setSGR [SetColor Foreground Dull Red]
            TIO.putStr "error: "
            setSGR [SetConsoleIntensity BoldIntensity]
            setSGR [SetColor Foreground Vivid White]
            TIO.putStrLn e
            setSGR [Reset]
        else do
            TIO.putStr "error: "

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

instance Show Command where
    show (SearchCommand Search{..}) =
        concat
            [ "Starting search from node "
            , nodeId
            , ", looking for resource "
            , resourceId
            , ", with TTL set to "
            , show ttl
            , ", with algorithm "
            , show algo
            ]
    show Help =
        "Use \"search\" with the nodeId, resourceId, ttl and algo\n\
        \Use \"quit\" to exit the program"

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

parseCommand :: Parser Command
parseCommand =
    try parseSearch <|> try parseHelp
