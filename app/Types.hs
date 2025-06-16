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
type Nodes = Map Text [Text]
type Node = Text
type Visited = Set.Set [Text]

data SearchAlgorithm
    = Flooding
    | InformedFlooding
    | RandomWalk
    | InformedRandomWalk
    deriving (Show,Eq)

data Search = Search
    { nodeId :: Text
    , nodeParentId :: Maybe Text
    , resourceId :: Text
    , ttl :: Int
    , algo :: SearchAlgorithm
    , found :: Bool
    , visited :: Visited
    , cache :: Map.Map Text [Text]
    }
    deriving (Show)

type SearchFound = Bool
type SearchResults = [Search]

newtype SearchStrategy = SearchStrategy
    { selectNextNodes :: Nodes -> Set.Set Text -> StdGen -> ([Text], StdGen)
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
                , visited = Set.empty
                , cache = Map.empty
                }

parseCommand :: Parser Command
parseCommand = do
    space
    choice
        [ parseSearch
        , string' "help" >> return Help
        , string' "list" >> return List
        , string' "algo" >> return Algo
        , string' "quit" >> return Quit
        , string' "reload" >> return Reload
        , string' "render" >> return Render
        ]

showSearch :: Search -> Text
showSearch Search{..} =
    T.pack $
        printf
            "[%s] [TTL=%d]: %s -> %s (%s)"
            (showAlgo algo)
            ttl
            (fromMaybe nodeId nodeParentId)
            nodeId
            ( if found
                then ("resource found" :: Text)
                else if ttl == 0 then "dead" else "searching"
            )

showAlgo :: SearchAlgorithm -> Text
showAlgo Flooding = "flooding"
showAlgo InformedFlooding = "informed_flooding"
showAlgo RandomWalk = "random_walk"
showAlgo InformedRandomWalk = "informed_random_walk"

fmtEdges :: Edges -> Text
fmtEdges edges =
    let adj = buildAdjacencyFromEdges edges
     in fmtMapList $ Map.toList adj

fmtResources :: Resources -> Text
fmtResources res =
    fmtMapList $ Map.toList res

fmtMapList :: [(Text, [Text])] -> Text
fmtMapList list =
    T.intercalate
        "\n"
        [k <> " => " <> fmtList v | (k, v) <- list]

fmtList :: [Text] -> Text
fmtList list = "[" <> T.intercalate "," list <> "]"
