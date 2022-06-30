module Main where

import Lucid

main :: IO ()
main = renderToFile "dist/index.html" masterHtml

masterHtml :: Html ()
masterHtml = p_ "hello world"
