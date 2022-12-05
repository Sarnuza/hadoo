module Hadoo.Lanes where

import Web.Scotty 
import qualified Data.Text.Lazy as LT
import Hadoo.HtmlUtils

showLanes :: ActionM ()
showLanes = htmlString $ "<div class='container'>" ++ unwords createLanes ++ "</div>"

createLanes :: [Html]
createLanes = do
    let state = "todo"
    ["<div class='lane'> <div class='title'>" ++ state ++ "</div>" ++ unwords createItems ++ "</div>"]

createItems :: [Html]
createItems = do
    ["<div class='item'>Test 123</div>"]
