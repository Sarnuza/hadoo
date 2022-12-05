module Hadoo.LanesItem where

import Web.Scotty 
import qualified Data.Text.Lazy as LT

showLanes :: ActionM ()
showLanes = htmlString "<div class='container'>Test</div>"



htmlString :: String -> ActionM ()
htmlString = html . LT.pack