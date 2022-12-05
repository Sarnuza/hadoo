module Hadoo.Persistance where

import Web.Scotty
import qualified Data.Text.Lazy as LT
import Control.Monad.IO.Class (liftIO)
import System.Directory (listDirectory, doesDirectoryExist, createDirectory, doesFileExist, renameFile, removeFile)
import Hadoo.State (State)
import Hadoo.HtmlUtils

getDirectoryPath :: State -> FilePath
getDirectoryPath state = "data/" ++ show state

getItemByState :: State -> IO [(String, String)]
getItemByState state = do
    filepath <- listDirectory (getDirectoryPath state)
    mapM readItem filepath

readItem ::  FilePath -> IO (String, String)
readItem path = do
    content <- readFile path
    return (path, content)