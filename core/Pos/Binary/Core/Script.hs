module Pos.Binary.Core.Script () where

import           Universum

import qualified Data.Store         as Store
import qualified PlutusCore.Program as PLCore
import qualified PlutusCore.Term    as PLCore

import           Pos.Binary.Class   (Bi (..), UnsignedVarInt (..), label, labelP, labelS,
                                     putField)
import           Pos.Core.Script    ()
import           Pos.Core.Types     (Script (..))

instance Bi PLCore.Term where
    size = Store.size
    {-# INLINE size #-}
    get = label "PLCore.Term" $ Store.peek
    {-# INLINE get #-}
    put = labelP "PLCore.Term" . Store.poke
    {-# INLINE put #-}

instance Bi PLCore.Program where
    size = Store.size
    {-# INLINE size #-}
    get = label "PLCore.Program" $ Store.peek
    {-# INLINE get #-}
    put = labelP "PLCore.Program" . Store.poke
    {-# INLINE put #-}

instance Bi Script where
    get = label "Script" $ do
        UnsignedVarInt scrVersion <- get
        scrScript                 <- get
        pure Script{..}
    sizeNPut = labelS "Script" $
        putField (UnsignedVarInt . scrVersion) <>
        putField scrScript
