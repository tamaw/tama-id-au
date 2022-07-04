module Main where

import Lucid
import Style
import qualified Clay
import Data.Text (Text)
import qualified Data.ByteString as BS
import qualified Data.List as List
import Data.Maybe (fromJust)
import System.Directory ( listDirectory )
import System.FilePath ( (</>), takeBaseName, takeExtension )
import Data.Char ( toLower )
import Control.Monad (forM_)
import Data.ByteString.Base64

type Uri = Text
type FileName = String
type Svg = Html ()

imagesFilePath, svgIconsFilePath :: FilePath
imagesFilePath = "resources" </> "images"
svgIconsFilePath = "resources" </> "icons"

data Model = Model
  { mySocialLinks :: [(Uri, Svg)]
  , myProfilePic :: Html ()
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
  svgsFiles <- listDirectory svgIconsFilePath
  svgs <- mapM (loadSvg . (svgIconsFilePath </>)) svgsFiles

  imagesFiles <- listDirectory imagesFilePath
  imgs <- mapM (loadImg . (imagesFilePath </>)) imagesFiles

  -- setup the model
  let model = Model
        { mySocialLinks =
          [ ("https://github.com", lookupFile GitHub svgs)
          , ("https://linked.in", lookupFile LinkedIn svgs)
          , ("https://stackvoerflow.com", lookupFile StackOverflow svgs)
          , ("https://twitter.com", lookupFile Twitter svgs)
          ]
          , myProfilePic = lookupFile "profile" imgs
        }

  -- write out html
  renderToFile "dist/index.html" $ masterHtml model
  where
    -- lookupFile name files = fromJust $ List.lookup (map toLower (show name)) files
    lookupFile name files = fromJust $ List.lookup (filter (/='"') (map toLower (show name))) files

loadSvg :: FilePath -> IO (FileName, Html ())
loadSvg filePath = do
  content <- BS.readFile filePath
  return (takeBaseName filePath, toHtmlRaw content)

loadImg :: FilePath -> IO (FileName, Html ())
loadImg filePath = do
  content <- BS.readFile filePath
  let imageType = getImageType $ takeExtension filePath
  let base64 = "base64," <> encodeBase64 content
  return (takeBaseName filePath, img_ [src_ $ imageType <> base64])
  where
    getImageType :: FilePath -> Text
    getImageType f
      | f == ".jpg" || f == "jpeg" = "data:image/jpeg;"
      | f == ".png" = "data:image/png;"
      | otherwise = error $ "Unknown image format for: " <> f

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
        div_ [class_ "flex flex-row"] $ do
           myProfilePic m
           h3_ "Tama Waddell"

        div_ [class_ "flex flex-row", id_ "landing-socialmedia"] $ do
            forM_ (mySocialLinks m) createSocialMediaLink
  where
    createSocialMediaLink :: (Uri, Svg) -> Html ()
    createSocialMediaLink (uri, svg) =
     div_ [class_ "flex flex-col", id_ "landing-socialmedia-item", style_ "--hover-color: orangered"] $
      a_ [href_ uri] svg

