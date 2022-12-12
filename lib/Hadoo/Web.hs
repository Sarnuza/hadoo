{-# LANGUAGE OverloadedStrings #-}

module Hadoo.Web where

import Web.Scotty 
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy as LT
import Hadoo.Lanes (showLanes)
import Hadoo.HtmlUtils
import Hadoo.Item (showNewItem, itemHtml)
import Hadoo.Persistance

main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev
  get "/styles.css" styles 
  get "/" indexAction
  get "/new" newPageAction
  get "/items" indexAction
  get "/items/:state/:nr/edit" editAction
  post "/items" newItemAction
  post "/items/:state/:nr/move/:nextState" moveItemAction
  post "/items/:state/:nr/delete" deleteItemAction
  post "/items/:state/:nr/" updateAction

styles :: ActionM ()
styles = do
    setHeader "Content-Type" "text/css"
    file "static/styles.css"

-- | Diese Funktion entfernt `\r` Control Characters aus den übertragenen Daten.
-- Sie müssen diese Funktion verwenden um Multiline Textinput ("content") aus einer 
-- Textarea auszulesen.
multiLineTextParam :: String -> ActionM String
multiLineTextParam paramName = fmap (filter (/='\r')) (param (LT.pack paramName)) 

indexAction :: ActionM () 
indexAction = do
    lanes <- liftIO showLanes
    htmlString lanes
            
newPageAction :: ActionM ()
newPageAction = htmlString showNewItem
    

newItemAction :: ActionM ()
newItemAction = do
    content <- multiLineTextParam "content"
    state <- param "state"
    liftIO (writeItem state content)
    redirect "/items"

moveItemAction :: ActionM ()
moveItemAction = do
    state <- param "state"
    nr <- param "nr"
    nextState <- param "nextState"
    liftIO (moveItem state nr nextState)
    redirect "/items"

deleteItemAction :: ActionM ()
deleteItemAction = do
    state <- param "state"
    nr <- param "nr"
    liftIO (deleteItem state nr)
    redirect "/items"

editAction :: ActionM ()
editAction = do
    state <- param "state"
    nr <- param "nr"
    item <- liftIO (getItemByStateAndNr state nr)
    htmlString (itemHtml (read state) nr (snd item))

updateAction :: ActionM ()
updateAction = do
    state <- param "state"
    nr <- param "nr"
    content <- multiLineTextParam "content"
    liftIO (updateItem state nr content)
    redirect "/items"