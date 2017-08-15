module Explorer.Util.Config where

import Prelude

import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Location (hostname) as L
import DOM.HTML.Window (location)
import Data.Generic (class Generic, gEq, gShow)
import Data.Maybe (isJust)
import Data.String.Regex (search)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)

foreign import versionImpl :: String

version :: String
version = versionImpl

testNetVersion :: String
testNetVersion = "0.5"

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

data SyncAction = SyncByPolling | SyncBySocket
derive instance gSyncAction :: Generic SyncAction
instance eqSyncAction :: Eq SyncAction where
    eq = gEq
instance showSyncAction :: Show SyncAction where
    show = gShow

syncBySocket :: SyncAction -> Boolean
syncBySocket = (==) SyncBySocket

syncByPolling :: SyncAction -> Boolean
syncByPolling = (==) SyncByPolling

isTestnet :: String -> Boolean
isTestnet location =
  isJust $ search (unsafeRegex "testnet\\." noFlags) location
