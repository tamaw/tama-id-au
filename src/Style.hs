module Style where

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
        border solid (px 1) auto

    "#landing-wrapper" ? do
        padding (em 2) (em 2) (em 2) (em 2)
        boxShadow . pure . bsColor (setA 0.7 grey) $ shadowWithSpread 0 0 (px 12) 0
        borderRadius (px 15) (px 15) (px 15) (px 15)
        maxWidth (px 300)
    "#landing-container" ? do
        border solid (px 1) auto
        img ? Clay.filters [grayscale  (pct 100), brightness 1.2]
        hr ? width (pct 100)
        h3 ? do
          sym margin auto
          sym padding (em 0.5)
    "#landing-socialmedia" ? do
        flexFlow row F.wrap
        justifyContent center
        border solid (px 1) auto
        borderColor black
    "#landing-socialmedia-item" ? do
        -- display inline
        sym borderRadius (px 4)
        sym margin (em 0.5)
        padding (px 3)(px 3)(px 0)(px 3)
        width (px 36)
        border solid (px 1) auto
        backgroundColor white
        hover & do
            boxShadow . pure . bsColor (setA 0.9 grey) $ shadowWithSpread 5 5 (px 10) 0
    "#landing-profile-image" ? do
        marginLeft auto
        marginRight auto

        maxWidth (pct 100)
        sym borderRadius (pct 15)

        boxShadow . pure . bsColor (setA 0.9 grey) $ shadowWithSpread 1 1 (px 2) 1
