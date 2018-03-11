-- | Interface for the Misc DB

module Pos.Update.DB.Misc
       ( isUpdateInstalled
       , affirmUpdateInstalled
       , getLastInstallerHash
       , setLastInstallerHash
       ) where

import           Universum

import           Formatting (sformat)

import           Pos.Binary.Class (Raw)
import           Pos.Crypto (Hash, hashHexF)
import           Pos.DB.Class (MonadDB, MonadDBRead)
import           Pos.DB.Misc.Common (miscGetBi, miscPutBi)

isUpdateInstalled :: MonadDB m => Hash Raw -> m Bool
isUpdateInstalled h = isJust <$> miscGetBi @() (updateTrackKey h)

affirmUpdateInstalled :: MonadDB m => Hash Raw -> m ()
affirmUpdateInstalled h = miscPutBi (updateTrackKey h) ()

updateTrackKey :: Hash Raw -> ByteString
updateTrackKey h = "updinst/" <> encodeUtf8 (sformat hashHexF h)

lastInstallerKey :: ByteString
lastInstallerKey = "updlast/lastDownloaded" 

getLastInstallerHash :: MonadDBRead m => m (Maybe (Hash Raw))
getLastInstallerHash = miscGetBi lastInstallerKey

setLastInstallerHash :: MonadDB m => Hash Raw -> m ()
setLastInstallerHash = miscPutBi lastInstallerKey