module Main where

import Lucid
import Style
import qualified Clay

main :: IO ()
main = renderToFile "dist/index.html" masterHtml

masterHtml :: Html ()
masterHtml = do
  doctype_
  html_ [lang_ "en"] $ do
    head_ headHtml
    body_ $ do
      main_ landingHtml

headHtml :: Html ()
headHtml = do
    title_ "Blog lol"
    meta_ [charset_ "UTF-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    meta_ [name_ "description", content_ "replace me" ]
    style_ [type_ "text/css"] $ Clay.render styleSheet
    -- dev only
    -- meta_ [httpEquiv_ "refresh", content_ "1"]

landingHtml :: Html ()
landingHtml = do
  div_ [class_ "flex flex-col items-center justify-center h-screen"] $
    div_ [class_ "wrapper-container", id_ "landing-wrapper"] $ do
      div_ [class_ "container", id_ "landing-container"] $ do
        div_ [class_ "flex-row"] $ span_ "Tama Waddell"
        div_ [class_ "flex-row"] $ do
          span_ "a"
          span_ "b"

