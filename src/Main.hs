module Main where

import Lucid
import Style
import qualified Clay
import Data.Text (Text)
import qualified Data.ByteString as BS
import qualified Data.List as List
import Data.Maybe (fromJust)
import System.Directory ( listDirectory )
import System.FilePath ( (</>), takeBaseName )
import Data.Char ( toLower )
import Control.Monad (forM_)

type Uri = Text
type FileName = String
type Svg = Html ()

svgIconsFilePath :: FilePath
svgIconsFilePath = "resources" </> "icons"

data Model = Model
  { mySocialLinks :: [(Uri, Svg)]
  } deriving (Show)

data SocialPlatform =
  GitHub
  | Twitter
  | LinkedIn
  | StackOverflow
  deriving (Enum, Bounded, Eq, Show)

main :: IO ()
main = do
  -- load all resources
  files <- listDirectory svgIconsFilePath
  socialMediaSvgs <- mapM (getSvg . (svgIconsFilePath </>)) files
  -- socialMediaSvgs <- mapM (getSvg . getFileName) [(minBound :: SocialPlatform) ..]

  -- setup the model
  let model = Model
        { mySocialLinks =
          [ ("https://github.com", lookupSocialMediaIcon GitHub socialMediaSvgs)
          , ("https://linked.in", lookupSocialMediaIcon LinkedIn socialMediaSvgs)
          , ("https://stackvoerflow.com", lookupSocialMediaIcon StackOverflow socialMediaSvgs)
          , ("https://twitter.com", lookupSocialMediaIcon Twitter socialMediaSvgs)
          ]
        }

  -- write out html
  renderToFile "dist/index.html" $ masterHtml model
  where
    lookupSocialMediaIcon sp svgs = fromJust $ List.lookup (map toLower (show sp)) svgs

    getSvg :: FileName -> IO (FileName, Html ())
    getSvg filePath = do
      content <- BS.readFile filePath
      return (takeBaseName filePath, toHtmlRaw content)

masterHtml :: Model -> Html ()
masterHtml m = do
  doctype_
  html_ [lang_ "en"] $ do
    head_ $ headHtml m
    body_ $ do
      main_ $ landingHtml m

headHtml :: Model -> Html ()
headHtml _m = do
    title_ "Blog lol"
    meta_ [charset_ "UTF-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    meta_ [name_ "description", content_ "replace me" ]
    style_ [type_ "text/css"] $ Clay.render styleSheet
    -- workaround: no fill element
    style_ [type_ "text/css"] ("svg:hover { fill: var(--hover-color); }" :: Text)

landingHtml :: Model -> Html ()
landingHtml m = do
  div_ [class_ "flex flex-col items-center justify-center h-screen"] $
    div_ [class_ "wrapper-container", id_ "landing-wrapper"] $ do
      div_ [class_ "container", id_ "landing-container"] $ do
        div_ [class_ "flex flex-row"] $ h3_ "Tama Waddell"
        div_ [class_ "flex flex-row", id_ "landing-socialmedia"] $ do
            forM_ (mySocialLinks m) createSocialMediaLink
  where
    createSocialMediaLink :: (Uri, Svg) -> Html ()
    createSocialMediaLink (uri, svg) =
     div_ [class_ "flex flex-col", id_ "landing-socialmedia-item", style_ "--hover-color: orangered"] $
      a_ [href_ uri] svg

