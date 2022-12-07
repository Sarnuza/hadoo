module Hadoo.Lanes where

import Web.Scotty
import Hadoo.HtmlUtils
import Hadoo.State
import Hadoo.Persistance
import Control.Monad.IO.Class (liftIO)



showLanes :: IO Html
showLanes = do
  lanes <- createLanes
  return ("<!DOCTYPE html> <html lang='en'> <!-- Styles importieren --> <head><link rel='stylesheet' href='styles.css'> </link></head>" ++ ea "div" [("class", "container")] (unwords lanes))

createLanes :: IO [Html]
createLanes = do
    mapM createlane (enumFromTo minBound maxBound)

createlane :: State -> IO Html
createlane state = do
  items <- createItems state
  return (ea "div" [("class", "lane")] (unwords [ea "div" [("class", "title")] (show state), items]))

createItems :: State -> IO Html
createItems state = do createItem state

createItem :: State -> IO Html
createItem state = do
    items <- getItemByState state
    let htmlItems = map (itemHtml state) items
    return (unwords htmlItems)
    

itemHtml :: State ->  (String, String) -> Html
itemHtml state (a, b) = ea "div" [("class", "item")] (b ++ show state ++ show 0 ++ itemButtons state 0)

itemButtons :: State -> Int -> Html
itemButtons state id
  | state == minBound = getRightArrow id ++ getEditAndDeleteButtons id
  | state == maxBound = getLeftArrow id ++ getEditAndDeleteButtons id
  | otherwise = getLeftArrow id ++ getRightArrow id ++ getEditAndDeleteButtons id

getRightArrow :: Int -> Html
getRightArrow id = "<form method='post' action='/items/Todo/" ++ show id ++ "/move/Started' class='inline'>"
                ++ "<button type='submit'>&gt;</button>"
                ++ "</form>"
getLeftArrow :: Int -> Html
getLeftArrow id = "<form method='post' action='/items/Todo/" ++ show id ++ "/move/Started' class='inline'>"
                ++ "<button type='submit'>&lt;</button>"
                ++ "</form>"

getEditAndDeleteButtons :: Int -> Html
getEditAndDeleteButtons id = "<form method='get' action='/items/" ++ show id ++ "/edit' class='inline' >"
                ++ "<button type='submit'>Edit</button>"
                ++ "</form>"
                ++ "<form method='post' action='/items/" ++ show id ++ "/delete' class='inline'>"
                ++ "<button type='submit'>Delete</button>"
                ++ "</form>"
