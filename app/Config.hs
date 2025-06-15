{-# LANGUAGE OverloadedStrings #-}

module Config where

import TOML
import Types
import Validator

readConfigFromArgs :: [String] -> IO (Either [Err] Config)
readConfigFromArgs [] = return (Left [Err "no config file"])
readConfigFromArgs (x : _) = do
    readConfig x

readConfig :: FilePath -> IO (Either [Err] Config)
readConfig cfgPath = do
    file <- decodeFile cfgPath
    return $ case file of
        Right cfg -> validateTOML cfg
        Left err -> Left [Err $ renderTOMLError err]

renderConfig :: Config -> IO ()
renderConfig cfg = undefined
