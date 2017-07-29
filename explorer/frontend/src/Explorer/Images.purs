module Explorer.Images where

import Prelude ((<>))

imagePath :: String -> String
imagePath = (<>) "/images/"

examplePath :: String
examplePath = imagePath "any-image.jpg"
