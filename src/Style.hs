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

    ".clear" ? clear both
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
        boxShadow . pure . bsColor (setA 0.7 grey)
         $ shadowWithSpread 0 0 (px 12) 0

    "#landing-container" ? do
        backgroundColor red