module Explorer.Util.QrCode
  ( mkQRCode
  , generateQrCode
  , QRCode
  ) where

import DOM (DOM)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, runEffFn1, runEffFn2)
import Prelude (Unit, bind, pure, unit)

data QRCode

type Config =
  { text :: String
  , width :: Int
  , height :: Int
  , colorDark :: String
  , colorLight :: String
  , typeNumber :: Int
  }

defaultConfig :: Config
defaultConfig =
  { text: ""
  , width: 256
  , height: 256
  , colorDark: "#000000"
  , colorLight: "#FFFFFF"
  , typeNumber: 4
  }

foreign import clearDomNodeImpl :: forall eff. EffFn1 (dom :: DOM | eff) String Unit
foreign import mkQRCodeImpl :: forall eff. EffFn2 (dom :: DOM | eff) String Config QRCode
foreign import clearImpl :: forall eff. EffFn1 (dom :: DOM | eff) QRCode QRCode


clearDomNode :: forall eff. String -> Eff (dom :: DOM | eff) Unit
clearDomNode = runEffFn1 clearDomNodeImpl

mkQRCode :: forall eff. String -> Config -> Eff (dom :: DOM | eff) QRCode
mkQRCode = runEffFn2 mkQRCodeImpl

clear :: forall eff. QRCode -> Eff (dom :: DOM | eff) QRCode
clear = runEffFn1 clearImpl

generateQrCode :: forall eff. String -> String -> Eff (dom :: DOM | eff) Unit
generateQrCode text id = do
  _ <- clearDomNode id
  qrCode <- mkQRCode id defaultConfig { text = text, width = 96, height = 96 }
  pure unit
