module Types where
import Data.Text
import Lucid

type Uri = Text
type FileName = String
type Base64 = Text
type Svg = Html ()

data Route =
  Index
  | Blog
  | About
  | Light
  | Dark
  deriving (Bounded, Enum, Show)

data Model = Model
  { mySocialLinks :: [(Uri, Svg)]
  , myProfilePic :: Base64
  , myFavIcon :: Svg
  , myLogo :: Svg
  , myBackgroundImage :: Svg
  , myRoutes :: [Route]
  , myLogoFont :: Base64
  } deriving (Show)

