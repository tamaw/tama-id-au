module Main where

import Types
import Style ( styleSheet, styleSheetFonts )

import Lucid
import qualified Clay
import Data.Maybe (fromJust)
import Control.Monad (forM_)
import Data.Char (toLower)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as BS
import qualified Data.List as List
import System.Directory ( listDirectory )
import System.FilePath ( takeExtension, takeFileName, (</>), (<.>) )
import qualified Network.URI.Encode as Uri

imagesFilePath, svgIconsFilePath, fontFilePath, outputPath :: FilePath
imagesFilePath = "resources" </> "images"
svgIconsFilePath = "resources" </> "icons"
fontFilePath = "resources" </> "fonts"
outputPath = "dist"

main :: IO ()
main = do
  -- load all resources
  svgsFiles <- listDirectory svgIconsFilePath
  svgs <- mapM (loadSvg . (svgIconsFilePath </>)) svgsFiles

  imagesFiles <- listDirectory imagesFilePath
  imgs <- mapM (loadImg . (imagesFilePath </>)) imagesFiles

  fontFiles <- listDirectory fontFilePath
  fonts <- mapM (loadFont . (fontFilePath </>)) fontFiles

  let routes = [minBound :: Route ..]
  -- TODO add not found 404.html

  -- setup the model
  let model = Model
        { mySocialLinks =
          [ ("https://github.com/tamaw", lookupFile "github.svg" svgs)
          , ("https://dev.to/tamaw", lookupFile "devdotto.svg" svgs)
          , ("https://www.linkedin.com/in/tama-waddell", lookupFile "linkedin.svg" svgs)
          , ("https://twitter.com/tamawdev", lookupFile "twitter.svg" svgs)
          ]
          , myProfilePic = lookupFile "profile.jpg" imgs
          , myFavIcon = lookupFile "favicon.svg" svgs
          , myLogo = lookupFile "logo.svg" svgs
          , myBackgroundImage = lookupFile "background.svg" svgs
          , myRoutes = routes
          , myLogoFont = lookupFile "exo2-bold-webfont.woff2" fonts
        }

  -- write out html
  forM_ routes (\r -> renderToFile (outputPath </> routeToFileName r) $ masterHtml r model)
  where
    loadSvg = loadFile toHtmlRaw
    loadImg f = loadFile (\b -> getDataHeader f <> "base64," <> BS.encodeBase64 b) f
    loadFont = loadImg
    lookupFile name files = fromJust $ List.lookup name files

routeToFileName :: Route -> FileName
routeToFileName r =  map toLower (show r) <.> "html"

loadFile :: (BS.ByteString -> a) -> FilePath -> IO (FileName, a)
loadFile convert filePath = do
  content <- BS.readFile filePath
  return (takeFileName filePath, convert content)

getDataHeader :: FilePath -> Text
getDataHeader f = case takeExtension f of
  ".jpg" -> "data:image/jpeg;"
  ".jpeg" -> "data:image/jpeg;"
  ".png" -> "data:image/png;"
  ".woff2" -> "data:file/octet-stream;"
  ".svg" -> "data:image/svg+xml;" -- todo user with favicon
  _ -> error $ "Unknown image format for: " <> f

masterHtml :: Route -> Model -> Html ()
masterHtml r m = do
  doctype_
  html_ [lang_ "en"] $ do
    head_ $ headHtml m
    body_ $ do
      main_ $ case r of
          Index -> landingHtml m
          Blog -> return ()
          About -> return ()
          Dark -> do
            div_ [style_ "background-color: #090A0B" ] $ do
              div_ [style_ "height:30px"] $ myLogo m
            div_ [] $ myBackgroundImage m
          Light -> do
            div_ [class_ "light", style_ "height:40px"] $ myLogo m
            div_ [class_ "light"] $ myBackgroundImage m

headHtml :: Model -> Html ()
headHtml m = do
  title_ "Tama Waddell"
  meta_ [charset_ "UTF-8"]
  meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
  -- meta_ [name_ "description", content_ "replace me" ]
  link_ [rel_ "icon", href_ ("data:image/svg+xml," <> urlEncodedFavIco (myFavIcon m))]
  style_ [type_ "text/css"] $ T.replace "\n" "" $ toStrict (Clay.render styleSheet)
  style_ [type_ "text/css"] $ T.replace "\n" "" $ toStrict (Clay.render $ styleSheetFonts $ myLogoFont m)
  -- workaround: no fill css element
  style_ [type_ "text/css" ] ("svg:hover { fill: var(--hover-color); }" :: Text)
  where
    urlEncodedFavIco i = Uri.encodeText . toStrict $ renderText i

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

headerHtml :: Model -> Html ()
headerHtml m = do
  p_ "testing header"
  navHtml m

navHtml :: Model -> Html ()
navHtml m =
  nav_ $ do
    ul_ $ do forM_ (myRoutes m) renderNavItem
  where
    renderNavItem :: Route -> Html ()
    renderNavItem r = li_ $ a_ [href_ . T.pack $ routeToFileName r] (toHtml $ show r)


