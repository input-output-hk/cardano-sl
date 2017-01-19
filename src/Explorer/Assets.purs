module Explorer.Assets where

import Prelude ((<>))

imagePath :: String -> String
imagePath = (<>) "/images/"

logoPath :: String
logoPath = imagePath "cardano-logo.svg"
