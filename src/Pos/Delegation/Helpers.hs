-- | Helper functions related to delegation.

module Pos.Delegation.Helpers
       ( dlgVerifyPayload
       ) where

import           Universum

import           Control.Monad.Except (MonadError (throwError))

import           Pos.Core             (EpochIndex)
import           Pos.Crypto           (ProxySecretKey (..))
import           Pos.Delegation.Types (DlgPayload (getDlgPayload))

-- | Verify delegation payload without using GState. This function can
-- be used for block verification in isolation, also it can be used
-- for mempool verification.
dlgVerifyPayload :: MonadError Text m => EpochIndex -> DlgPayload -> m ()
dlgVerifyPayload epoch (getDlgPayload -> proxySKs) =
    unless (null notMatchingEpochs) $
    throwError "Block contains psk(s) that have non-matching epoch index"
  where
    notMatchingEpochs = filter ((/= epoch) . pskOmega) proxySKs
