module Hadoo.HtmlUtils where

import Web.Scotty
import qualified Data.Text.Lazy as LT
import Data.List (intersperse)

-- | Type Alias für Html Strings
type Html = String

htmlString :: String -> ActionM ()
htmlString = html . LT.pack

-- | Erzeugt ein Element ohne Attribute
e :: String -> Html -> Html
e tag = ea tag []

-- | Erzeugt ein Element mit Attributen
ea :: String -> [(String, String)] -> Html -> Html
ea tag attrs kids = concat $ ["<", tag] ++ attrsHtml attrs ++ [">", kids, "</", tag, ">"]
  where attrsHtml [] = []
        attrsHtml as = " " : intersperse " " (map attrHtml as)
        attrHtml (key, value) = key ++ "='" ++ value ++ "'"
