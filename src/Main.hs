module Main where

import Lucid

main :: IO ()
main = renderToFile "dist/index.html" masterHtml

masterHtml :: Html ()
masterHtml =
  doctypehtml_ $ do
    head_ headHtml
    body_ $
      main_ landingHtml

headHtml :: Html ()
headHtml = do
    title_ "Blog lol"
    meta_ [charset_ "UTF-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    meta_ [name_ "description", content_ "replace me" ]
    -- dev only
    meta_ [httpEquiv_ "refresh", content_ "1"]

landingHtml :: Html ()
landingHtml = do
  p_ "land here"
