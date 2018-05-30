{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Delegation types serialization.

module Pos.Binary.Delegation
       (
       ) where

import           Universum

import           Pos.Binary.Class (Bi (..), Cons (..), Field (..), deriveSimpleBi)
import           Pos.Binary.Core ()
import           Pos.Binary.Crypto ()
import           Pos.Communication.Types.Relay (DataMsg (..))
import           Pos.Core (ProxySKHeavy, StakeholderId)
import           Pos.Delegation.Types (DlgUndo (..))

deriveSimpleBi ''DlgUndo [
    Cons 'DlgUndo [
        Field [| duPsks            :: [ProxySKHeavy]        |],
        Field [| duPrevEpochPosted :: HashSet StakeholderId |]
    ]]

instance Bi (DataMsg ProxySKHeavy) where
    encode = encode . dmContents
    decode = DataMsg <$> decode
