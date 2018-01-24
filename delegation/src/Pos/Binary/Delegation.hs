-- | Delegation types serialization.

module Pos.Binary.Delegation
       (
       ) where

import           Universum

import           Pos.Binary.Class (Bi (..), Cons (..), Field (..), deriveSimpleBiCxt)
import           Pos.Binary.Core ()
import           Pos.Binary.Crypto ()
import           Pos.Communication.Types.Relay (DataMsg (..))
import           Pos.Core (ProxySKHeavy, StakeholderId)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Delegation.Types (DlgUndo (..))

deriveSimpleBiCxt [t|HasConfiguration|] ''DlgUndo [
    Cons 'DlgUndo [
        Field [| duPsks            :: [ProxySKHeavy]        |],
        Field [| duPrevEpochPosted :: HashSet StakeholderId |]
    ]]

instance HasConfiguration => Bi (DataMsg ProxySKHeavy) where
    encode = encode . dmContents
    decode = DataMsg <$> decode
