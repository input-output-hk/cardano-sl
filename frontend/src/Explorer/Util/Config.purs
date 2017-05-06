module Explorer.Util.Config where

import Control.Bind ((>>=))
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Location (hostname) as L
import DOM.HTML.Window (location)

foreign import versionImpl :: Int

version :: Int
version = versionImpl

foreign import commitHashImpl :: String

commitHash :: String
commitHash = commitHashImpl

foreign import isProductionImpl :: Boolean

isProduction :: Boolean
isProduction = isProductionImpl

hostname :: forall eff. Eff (dom :: DOM | eff) String
hostname = window >>= location >>= L.hostname

data Protocol = Http | Https

secureProtocol :: Boolean -> Protocol
secureProtocol true  = Https
secureProtocol false = Http
