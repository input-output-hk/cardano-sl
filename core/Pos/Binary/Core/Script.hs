module Pos.Binary.Core.Script () where

import qualified Data.Binary        as Binary
import qualified PlutusCore.Program as PLCore
import qualified PlutusCore.Term    as PLCore
import           Universum

import           Pos.Binary.Class   (Bi (..), UnsignedVarInt (..), label)
import           Pos.Core.Script    ()
import           Pos.Core.Types     (Script (..))

instance Bi PLCore.Term where
  get = undefined -- CSL-1122 uncomment -- Binary.get
  {-# INLINE get #-}
  put = undefined -- CSL-1122 uncomment -- Binary.put
  {-# INLINE put #-}

instance Bi PLCore.Program where
  get = undefined -- CSL-1122 uncomment -- Binary.get
  {-# INLINE get #-}
  put = undefined -- CSL-1122 uncomment -- Binary.put
  {-# INLINE put #-}

instance Bi Script where
    get = label "Script" $ do
        UnsignedVarInt scrVersion <- get
        scrScript <- get
        return Script{..}
    put Script{..} = do
        put (UnsignedVarInt scrVersion)
        put scrScript
