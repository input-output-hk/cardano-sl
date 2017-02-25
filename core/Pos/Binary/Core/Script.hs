module Pos.Binary.Core.Script () where

import           Data.Binary.Get    (label)
import qualified PlutusCore.Program as PLCore
import qualified PlutusCore.Term    as PLCore
import           Universum

import           Pos.Binary.Class   (Bi (..), UnsignedVarInt (..))
import           Pos.Core.Script    ()
import           Pos.Core.Types     (Script (..))

instance Bi PLCore.Term
instance Bi PLCore.Program

instance Bi Script where
    get = label "Script" $ do
        UnsignedVarInt scrVersion <- get
        scrScript <- get
        return Script{..}
    put Script{..} = do
        put (UnsignedVarInt scrVersion)
        put scrScript
