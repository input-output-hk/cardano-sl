-- | Delegation types serialization.

module Pos.Binary.Delegation
       (
       ) where

import           Universum

import           Pos.Binary.Class           (Cons (..), Field (..), deriveSimpleBi)
import           Pos.Binary.Core            ()
import           Pos.Binary.Core.Delegation ()
import           Pos.Binary.Crypto          ()
import           Pos.Core                   (ProxySKHeavy, StakeholderId)
import           Pos.Delegation.Types       (DlgUndo (..))

deriveSimpleBi ''DlgUndo [
    Cons 'DlgUndo [
        Field [| duPsks            :: [ProxySKHeavy]        |],
        Field [| duPrevEpochPosted :: HashSet StakeholderId |]
    ]]
