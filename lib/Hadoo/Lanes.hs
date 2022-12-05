module Hadoo.Lanes where

import Web.Scotty
import Hadoo.HtmlUtils
import Hadoo.State


showLanes :: ActionM ()
showLanes = htmlString $ "<!DOCTYPE html> <html lang='en'> <!-- Styles importieren --> <head><link rel='stylesheet' href='styles.css'> </link></head>" ++ ea "div" [("class", "container")] (unwords createLanes)

createLanes :: [Html]
createLanes = do
    map createlane (enumFromTo minBound maxBound)

createlane :: State -> Html
createlane state = ea "div" [("class", "lane")] (unwords [ea "div" [("class", "title")] (show state), unwords (createItems state)])

createItems :: State -> [Html]
createItems state = map (createItem state) [1..10]

createItem :: State -> Int -> Html
createItem state id = ea "div" [("class", "item")] (show state ++ show id ++ itemButtons state id)

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
