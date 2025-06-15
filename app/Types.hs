{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void
import System.Console.ANSI
import System.IO (stdout)
import System.Random
import TOML
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Printf

type Parser = Parsec Void Text

newtype Err = Err Text

data Config = Config
    { minNeighbors :: Maybe Int
    , maxNeighbors :: Maybe Int
    , resources :: Resources
    , edges :: Edges
    }
    deriving (Show)

instance DecodeTOML Config where
    tomlDecoder =
        Config
            <$> getFieldOpt "min_neighbours"
            <*> getFieldOpt "max_neighbours"
            <*> getField "resources"
            <*> getField "edges"

type Edges = [[Text]]

buildAdjacencyFromEdges :: Edges -> Map.Map Text [Text]
buildAdjacencyFromEdges edges' =
    let
        expandEdge [a, b] = [(a, [b]), (b, [a])]
        expandEdge _ = []

        allPairs = concatMap expandEdge edges'

        merged = Map.fromListWith (++) allPairs
     in
        Map.map nub merged

type Resources = Map Text [Text]

data SearchAlgorithm
    = Flooding
    | InformedFlooding
    | RandomWalk
    | InformedRandomWalk
    deriving (Show)

data Search = Search
    { nodeId :: Text
    , nodeParentId :: Maybe Text
    , resourceId :: Text
    , ttl :: Int
    , algo :: SearchAlgorithm
    , found :: Bool
    }
    deriving (Show)

type SearchFound = Bool
type SearchResult = ([Search], SearchFound)

data SearchStrategy = SearchStrategy
    { selectNext :: [Text] -> Set.Set Text -> StdGen -> ([Text], StdGen)
    , order :: [Text] -> [Text]
    , useRandom :: Bool
    }

printErr :: Err -> IO ()
printErr (Err x) = do
    stdoutSupportsANSI <- hNowSupportsANSI stdout
    if stdoutSupportsANSI
        then do
            setSGR [SetConsoleIntensity BoldIntensity]
            setSGR [SetColor Foreground Dull Red]
            TIO.putStr "error: "
            setSGR [SetConsoleIntensity BoldIntensity]
            setSGR [SetColor Foreground Vivid White]
            TIO.putStrLn x
            setSGR [Reset]
        else do
            TIO.putStr "error: "
            TIO.putStrLn x

printErrs :: [Err] -> IO ()
printErrs = mapM_ printErr

data Command
    = Help
    | List
    | Render
    | SearchCommand Search
    | Algo
    | Reload
    | Quit
    deriving (Show)

identifier :: Parser Text
identifier = do
    first <- letterChar
    rest <- many (alphaNumChar <|> char '_')
    return $ T.pack (first : rest)

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

    return $
        SearchCommand $
            Search
                { nodeId = nodeId'
                , nodeParentId = Nothing
                , resourceId = resourceId'
                , ttl = ttl'
                , algo = algo'
                , found = False
                }

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

parseAlgo :: Parser Command
parseAlgo = do
    space
    _ <- string' "algo"
    return Algo

parseQuit :: Parser Command
parseQuit = do
    space
    _ <- string' "quit"
    return Quit

parseReload :: Parser Command
parseReload = do
    space
    _ <- string' "reload"
    return Reload

parseRender :: Parser Command
parseRender = do
    space
    _ <- string' "render"
    return Render

parseCommand :: Parser Command
parseCommand =
    try parseHelp
        <|> try parseList
        <|> try parseSearch
        <|> try parseAlgo
        <|> try parseReload
        <|> try parseQuit
        <|> try parseRender

showSearch :: Search -> Text
showSearch Search{..} =
    T.pack $
        printf
            "[%s] [TTL=%d]: %s -> %s (%s)"
            (showAlgo algo)
            ttl
            (fromMaybe nodeId nodeParentId)
            nodeId
            ( if ttl == 0
                then ("dead" :: Text)
                else if found then "resource found" else "searching"
            )

showAlgo :: SearchAlgorithm -> Text
showAlgo Flooding = "flooding"
showAlgo InformedFlooding = "informed_flooding"
showAlgo RandomWalk = "random_walk"
showAlgo InformedRandomWalk = "informed_random_walk"
