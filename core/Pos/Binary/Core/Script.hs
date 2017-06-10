module Pos.Binary.Core.Script () where

import           Universum

import qualified Data.Store         as Store
import qualified PlutusCore.Program as PLCore
import qualified PlutusCore.Term    as PLCore

import           Pos.Binary.Class   (Bi (..), UnsignedVarInt (..), combineSize, label)
import           Pos.Core.Script    ()
import           Pos.Core.Types     (Script (..))

instance Bi PLCore.Term where
  size = Store.size
  {-# INLINE size #-}
  get = Store.peek
  {-# INLINE get #-}
  put = Store.poke
  {-# INLINE put #-}

instance Bi PLCore.Program where
  size = Store.size
  {-# INLINE size #-}
  get = Store.peek
  {-# INLINE get #-}
  put = Store.poke
  {-# INLINE put #-}

instance Bi Script where
    size = combineSize (UnsignedVarInt . scrVersion, scrScript)
    get = label "Script" $ do
        UnsignedVarInt scrVersion <- get
        scrScript                 <- get
        pure Script{..}
    put Script{..} =
        put (UnsignedVarInt scrVersion)
     *> put scrScript
