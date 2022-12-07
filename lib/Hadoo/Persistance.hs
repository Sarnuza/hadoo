module Hadoo.Persistance where

import Web.Scotty
import qualified Data.Text.Lazy as LT
import Control.Monad.IO.Class (liftIO)
import System.Directory (listDirectory, createDirectoryIfMissing, doesDirectoryExist, createDirectory, doesFileExist, renameFile, removeFile)
import Hadoo.State (State)
import Hadoo.HtmlUtils

getOrCreateDirectoryPath :: State -> IO FilePath
getOrCreateDirectoryPath state =  do
    let path = "data/" ++ show state
    createDirectoryIfMissing True path
    return path

getItemByState :: State -> IO [(String, String)]
getItemByState state = do
    basePath <- getOrCreateDirectoryPath state
    filepath <- listDirectory basePath
    mapM (readItem basePath) filepath

readItem ::  FilePath -> FilePath -> IO (String, String)
readItem basePath path = do
    content <- readFile (basePath ++ "/" ++ path)
    return (path, content)