module Hadoo.Persistance where 

import Web.Scotty
import qualified Data.Text.Lazy as LT
import Control.Monad.IO.Class (liftIO)
import System.Directory (listDirectory, doesDirectoryExist, createDirectory, doesFileExist, renameFile, removeFile)

getTodo :: ActionM ()
getTodo = do
    let filepath = listDirectory "data/Done"
    
    htmlString "test"

htmlString :: String -> ActionM ()
htmlString = html . LT.pack