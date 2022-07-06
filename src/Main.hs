module Main where

import Lucid
import Style
import qualified Clay
import Data.Text (Text)
import qualified Data.ByteString as BS
import qualified Data.List as List
import Data.Maybe (fromJust)
import System.Directory ( listDirectory )
import System.FilePath ( (</>), takeExtension, takeFileName )
import Control.Monad (forM_)
import Data.ByteString.Base64
import qualified Network.URI.Encode as Uri
import qualified Data.Text.Lazy as T

type Uri = Text
type FileName = String
type Base64 = Text
type Svg = Html ()

-- data Svg =
  -- SvgXml (Html ())

imagesFilePath, svgIconsFilePath :: FilePath
imagesFilePath = "resources" </> "images"
svgIconsFilePath = "resources" </> "icons"

data Model = Model
  { mySocialLinks :: [(Uri, Svg)]
  , myProfilePic :: Base64
  , myFavIcon :: Svg
  } deriving (Show)

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
          [ ("https://github.com/tamaw", lookupFile "github.svg" svgs)
          , ("https://www.linkedin.com/in/tama-waddell-7218461b9/", lookupFile "linkedin.svg" svgs)
          , ("https://stackoverflow.com/users/4778435/tama", lookupFile "stackoverflow.svg" svgs)
          , ("https://twitter.com/twaddell_", lookupFile "twitter.svg" svgs)
          ]
          , myProfilePic = lookupFile "profile.jpg" imgs
          , myFavIcon = lookupFile "favicon.svg" svgs
        }

  -- write out html
  renderToFile "dist/index.html" $ masterHtml model
  where
    loadSvg = loadFile toHtmlRaw
    loadImg f = loadFile (\b -> getDataHeader f <> "base64," <> encodeBase64 b) f
    lookupFile name files = fromJust $ List.lookup name files

loadFile :: (BS.ByteString -> a) -> FilePath -> IO (FileName, a)
loadFile convert filePath = do
  content <- BS.readFile filePath
  return (takeFileName filePath, convert content)

getDataHeader :: FilePath -> Text
getDataHeader f = case takeExtension f of
  ".jpg" -> "data:image/jpeg;"
  ".jpeg" -> "data:image/jpeg;"
  ".png" -> "data:image/png;"
  ".svg" -> "data:image/svg+xml;" -- todo use for with favicon
  _ -> error $ "Unknown image format for: " <> f

masterHtml :: Model -> Html ()
masterHtml m = do
  doctype_
  html_ [lang_ "en"] $ do
    head_ $ headHtml m
    body_ $ do
      main_ $ landingHtml m

headHtml :: Model -> Html ()
headHtml m = do
  title_ "Blog lol"
  meta_ [charset_ "UTF-8"]
  meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
  meta_ [name_ "description", content_ "replace me" ]
  link_ [rel_ "icon", href_ ("data:image/svg+xml," <> urlEncodedFavIco (myFavIcon m))]
  style_ [type_ "text/css"] $ Clay.render styleSheet
  -- workaround: no fill css element
  style_ [type_ "text/css" ] ("svg:hover { fill: var(--hover-color); }" :: Text)
  where
    urlEncodedFavIco i = Uri.encodeText . T.toStrict $ renderText i

landingHtml :: Model -> Html ()
landingHtml m = do
  div_ [class_ "flex flex-col items-center justify-center h-screen"] $
    div_ [class_ "wrapper-container", id_ "landing-wrapper"] $ do
      div_ [class_ "container", id_ "landing-container"] $ do
        div_ [class_ "flex flex-row"] $ img_ [src_ $ myProfilePic m, id_ "landing-profile-image" ]
        div_ [class_ "flex flex-row"] $ h3_ "Tama Waddell"
        div_ [class_ "flex flex-row", id_ "landing-socialmedia"] $ do
          hr_ []
          forM_ (mySocialLinks m) renderSocialMediaLink
          hr_ []
  where
    renderSocialMediaLink :: (Uri, Svg) -> Html ()
    renderSocialMediaLink (uri, svg) =
     div_ [class_ "flex flex-col", id_ "landing-socialmedia-item", style_ "--hover-color: orangered"] $
      a_ [href_ uri] svg

