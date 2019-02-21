module Pos.DB.GState.Tip
       (
         -- * Getters
         getTip
       , getTipSomething

         -- * Putters
       , putTip

         -- * Keys
       , tipKey
       ) where

import           Universum

import           Formatting (sformat, stext, (%))

import           Pos.Chain.Block (HeaderHash)
import           Pos.DB.Class (MonadDB, MonadDBRead (..))
import           Pos.DB.Error (DBError (DBMalformed))
import           Pos.DB.GState.Common (gsGetBi, gsPutBi)
import           Pos.Util.Util (maybeThrow)

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

-- | Get current tip from GState DB.
getTip :: MonadDBRead m => m HeaderHash
getTip = maybeThrow (DBMalformed "no tip in GState DB") =<< getTipMaybe

getTipSomething
    :: forall m smth.
       MonadDBRead m
    => Text -> (HeaderHash -> m (Maybe smth)) -> m smth
getTipSomething smthDescription smthGetter =
    maybe onFailure pure =<< smthGetter =<< getTip
  where
    fmt = "there is no "%stext%" corresponding to tip"
    onFailure = throwM $ DBMalformed $ sformat fmt smthDescription

getTipMaybe :: MonadDBRead m => m (Maybe HeaderHash)
getTipMaybe = gsGetBi tipKey

----------------------------------------------------------------------------
-- Putters
----------------------------------------------------------------------------

putTip :: MonadDB m => HeaderHash -> m ()
putTip = gsPutBi tipKey

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

tipKey :: ByteString
tipKey = "c/tip"
