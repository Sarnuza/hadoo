module Hadoo.Item where

import Hadoo.HtmlUtils ( ea, Html )
import Hadoo.State

showNewItem :: Html
showNewItem = ea "div" [("class", "item")] newItemHtml

newItemHtml :: Html
newItemHtml = do
    let stateDropdown = ea "select" [("name", "state")] (unwords (map (selectOption minBound) (enumFromTo minBound maxBound)))
    let contentInput = ea "textarea" [("name", "content"), ("class", "content")] ""
    getPostNewForm [stateDropdown, contentInput]

getPostNewForm :: [Html] -> Html
getPostNewForm children = ea "form" [("method", "post"), ("action", "/items")] ((ea "button" [("type", "submit")] "Create") ++ unwords children)

itemHtml :: State -> String -> String -> Html
itemHtml state nr content = do
    let contentInput = ea "textarea" [("name", "content"), ("rows", "12"), ("cols", "60"), ("class", "content")] content
    getPostUpdateForm state nr contentInput

selectOption :: State -> State -> Html
selectOption selectedState state 
    | state == selectedState = ea "option" [("selected", "selected"), ("value", show state)] (show state)
    | otherwise = ea "option" [("value", show state)] (show state)

getPostUpdateForm :: State -> String -> Html -> Html
getPostUpdateForm state nr content = ea "form" [("method", "post"), ("action", "/items/" ++ show state ++ "/" ++ nr ++ "/")] ((ea "button" [("type", "submit")] "Update") ++ content)