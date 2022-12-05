{-# LANGUAGE OverloadedStrings #-}

module Hadoo.Web where

import Web.Scotty 
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy as LT
import Data.List (intersperse)
import Hadoo.Lanes (showLanes)
import Hadoo.HtmlUtils

main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev


  get "/styles.css" styles 
  get "/" indexAction
  get "/new" newPageAction
  get "/items" itemsPageAction
  get "/demo" demoPageAction

styles :: ActionM ()
styles = do
    setHeader "Content-Type" "text/css"
    file "static/styles.css"

-- | Diese Funktion entfernt `\r` Control Characters aus den übertragenen Daten.
-- Sie müssen diese Funktion verwenden um Multiline Textinput ("content") aus einer 
-- Textarea auszulesen.
multiLineTextParam :: String -> ActionM String
multiLineTextParam paramName = fmap (filter (/='\r')) (param (LT.pack paramName)) 

demoPageAction :: ActionM ()
demoPageAction = do
    demoPage <- liftIO (readFile "static/lanes_example.html")
    htmlString demoPage

indexAction :: ActionM () 
indexAction = do
            showLanes
            
newPageAction :: ActionM ()
newPageAction = do
    demoPage <- liftIO (readFile "static/new.html")
    htmlString demoPage

itemsPageAction :: ActionM ()
itemsPageAction = do
    demoPage <- liftIO (readFile "static/items.html")
    htmlString demoPage