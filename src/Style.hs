module Style where

import Data.Monoid
import Data.Text (Text)
import Clay
import qualified Clay.Flexbox as F


fontHeading, fontPrimary, fontSecondary :: Text
fontHeading = "Verdana"
fontPrimary = "Tahoma"
fontSecondary = "Segoe"

styleSheet :: Css
styleSheet = do
    star ? do
        boxSizing inherit
        before & boxSizing inherit
        after & boxSizing inherit
    html ? do
        boxSizing borderBox
        lineHeight (em 1.5)
    body ? do
        fontFamily [fontPrimary] [sansSerif]
        color black
        backgroundColor white

    ".flex" ? display flex
    ".flex-row" ? flexDirection row
    ".flex-col" ? flexDirection column

    ".items-center" ? alignItems center
    ".justify-center" ? justifyContent center
    ".h-screen" ? height (vh 100)
    ".relative" ? position relative
    ".wrapper-container" ? do
        width auto
        margin auto auto auto auto
    ".container" ? do
        backgroundColor tomato

    "#landing-wrapper" ? do
        padding (em 2) (em 2) (em 2) (em 2)
        boxShadow . pure . bsColor (setA 0.7 grey) $ shadowWithSpread 0 0 (px 12) 0
        borderRadius (px 15) (px 15) (px 15) (px 15)
    "#landing-container" ? do
        backgroundColor red
    "#landing-socialmedia" ? do
        flexFlow row F.wrap
    "#landing-socialmedia-item" ? do
        padding (px 4) (px 4) (px 4) (px 4)
        width (px 40)
        margin (em 0.5) (em 0)(em 0)(em 0)
        backgroundColor grey
        boxShadow . pure . bsColor (setA 0.7 grey) $ shadowWithSpread 0 0 (px 12) 0

