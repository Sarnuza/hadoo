module Hadoo.Persistance where

import System.Directory (listDirectory, createDirectoryIfMissing, removeFile)
import Text.Printf (printf)

getOrCreateDirectoryPath :: String -> IO FilePath
getOrCreateDirectoryPath state =  do
    let path = "data/" ++ state
    createDirectoryIfMissing True path
    return path

getItemByState :: String -> IO [(String, String)]
getItemByState state = do
    basePath <- getOrCreateDirectoryPath state
    filepath <- listDirectory basePath
    mapM (readItem basePath) filepath

getItemByStateAndNr :: String -> String -> IO (String, String)
getItemByStateAndNr state nr = do
    basePath <- getOrCreateDirectoryPath state
    readItem basePath (addLeadingZeros (read nr) ++ ".txt")

readItem ::  FilePath -> FilePath -> IO (String, String)
readItem basePath filename = do
    content <- readFile (basePath ++ "/" ++ filename)
    return (filename, content)

writeItem :: String -> String -> IO ()
writeItem state content = do
    basePath <- getOrCreateDirectoryPath state
    filename <- getNextFilename state
    let path = basePath ++ "/" ++ filename
    writeFile path content

updateItem :: String -> Int -> String -> IO ()
updateItem state id content = do
    basePath <- getOrCreateDirectoryPath state
    let path = basePath ++ "/" ++ addLeadingZeros id ++ ".txt"
    writeFile path content

deleteItem :: String -> String -> IO ()
deleteItem state nr = do
    basePath <- getOrCreateDirectoryPath state
    removeFile (basePath ++ "/" ++ (addLeadingZeros (read nr) ++ ".txt"))

moveItem :: String -> String -> String -> IO ()
moveItem state nr nextState = do
    basePath <- getOrCreateDirectoryPath state
    item <- readItem basePath (addLeadingZeros (read nr) ++ ".txt")
    deleteItem state nr

    writeItem nextState (snd item)

getNextFilename :: String -> IO FilePath
getNextFilename state = do
    basePath <- getOrCreateDirectoryPath state
    files <- listDirectory basePath
    if null files then return "000.txt"
    else do
        let numbers = map (read . takeWhile (/= '.')) files
        let nextNumber = maximum numbers + 1
        return (addLeadingZeros nextNumber ++ ".txt")

addLeadingZeros :: Int -> String
addLeadingZeros = printf "%03d"
