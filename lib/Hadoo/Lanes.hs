{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Hadoo.Lanes where

import Hadoo.HtmlUtils ( ea, Html )
import Hadoo.State
import Hadoo.Persistance

showLanes :: IO Html
showLanes = do
  lanes <- createLanes
  return ("<!DOCTYPE html> <html lang='en'> <!-- Styles importieren --> <head><link rel='stylesheet' href='styles.css'> </link></head>" ++ ea "a" [("href", "/new")] "New Item" ++ ea "div" [("class", "container")] (unwords lanes))

createLanes :: IO [Html]
createLanes = do
    mapM createlane (enumFromTo minBound maxBound)

createlane :: State -> IO Html
createlane state = do
  items <- createItems state 
  return (ea "div" [("class", "lane")] (unwords [ea "div" [("class", "title")] (show state) ++ show (length items), unwords items]))

createItems :: State -> IO [Html]
createItems state = do
    items <- getItemByState (show state)
    let htmlItems = map (itemHtml state) items
    return htmlItems

itemHtml :: State ->  (String, String) -> Html
itemHtml state (a, b) = ea "div" [("class", "item")] (b ++ itemButtons state (getIntFromFilePath a))

itemButtons :: State -> Int -> Html
itemButtons state id
  | state == minBound = getRightArrow id state ++ getEditAndDeleteButtons id state
  | state == maxBound = getLeftArrow id state ++ getEditAndDeleteButtons id state
  | otherwise = getLeftArrow id state ++ getRightArrow id state ++ getEditAndDeleteButtons id state

getRightArrow :: Int -> State -> Html
getRightArrow id state = do "<form method='post' action='/items/" ++ show state ++ "/" ++ show id ++ "/move/" ++ show (succ state) ++ "' class='inline'>"
                ++ "<button type='submit'>&gt;</button>"
                ++ "</form>"
getLeftArrow :: Int -> State -> Html
getLeftArrow id state = "<form method='post' action='/items/" ++ show state ++ "/" ++ show id ++ "/move/" ++ show (pred state) ++ "' class='inline'>"
                ++ "<button type='submit'>&lt;</button>"
                ++ "</form>"

getEditAndDeleteButtons :: Int -> State -> Html
getEditAndDeleteButtons id state = "<form method='get' action='/items/" ++ show state ++"/" ++ show id ++ "/edit' class='inline' >"
                ++ "<button type='submit'>Edit</button>"
                ++ "</form>"
                ++ "<form method='post' action='/items/" ++ show state ++"/" ++ show id ++ "/delete' class='inline'>"
                ++ "<button type='submit'>Delete</button>"
                ++ "</form>"

getIntFromFilePath :: String -> Int
getIntFromFilePath = read . takeWhile (/= '.') . drop 1