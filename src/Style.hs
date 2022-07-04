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
        border solid (px 1) auto
        borderColor black
        backgroundColor orangered
    "#landing-socialmedia-item" ? do
        -- display inline
        sym borderRadius (px 4)
        sym margin (em 0.5)
        padding (px 3)(px 3)(px 0)(px 3)
        width (px 36)
        -- margin (em 0.5) (em 0.5)(em 0.5)(em 0.5)
        border solid (px 1) auto
        -- borderColor orangered
        backgroundColor white
        -- Clay.filters [brightness 0, saturate (pct 100)]
        hover & do
            boxShadow . pure . bsColor (setA 0.9 grey) $ shadowWithSpread 5 5 (px 10) 0
    "#landing-profile-image" ? do
        marginLeft auto
        marginRight auto
        


