{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Common methods/datatypes across Delegation.Logic.*

module Pos.Delegation.Logic.Common
       (
       -- * Exceptions
         DelegationError(..)

       -- * Modifying memstate
       , DelegationStateAction
       , runDelegationStateAction

       -- * Common helpers
       , getDlgTransPsk
       ) where

import           Universum

import           Control.Exception.Safe (Exception (..))
import qualified Data.Text.Buildable as B
import           Formatting (bprint, stext, (%))
import           UnliftIO (MonadUnliftIO)

import           Pos.Core (ProxySKHeavy, StakeholderId)
import           Pos.Crypto (ProxySecretKey (..), PublicKey)
import           Pos.DB (MonadDBRead)
import           Pos.Delegation.Cede (dlgLastPsk, getPsk, runDBCede)
import           Pos.Delegation.Class (DelegationWrap (..), MonadDelegation,
                     askDelegationState)
import           Pos.Exception (cardanoExceptionFromException,
                     cardanoExceptionToException)

----------------------------------------------------------------------------
-- Exceptions
----------------------------------------------------------------------------

data DelegationError =
    -- | Can't apply blocks to state of transactions processing.
    DelegationCantApplyBlocks Text
    deriving (Typeable, Show)

instance Exception DelegationError where
    toException = cardanoExceptionToException
    fromException = cardanoExceptionFromException
    displayException = toString . pretty

instance B.Buildable DelegationError where
    build (DelegationCantApplyBlocks msg) =
        bprint ("can't apply in delegation module: "%stext) msg

----------------------------------------------------------------------------
-- Modifying memstate
----------------------------------------------------------------------------

-- | Convenient monad to work in 'DelegationWrap' state context.
type DelegationStateAction = State DelegationWrap

-- | Executes atomic action on delegation variable.
runDelegationStateAction
    :: (MonadIO m, MonadDelegation ctx m)
    => DelegationStateAction a -> m a
runDelegationStateAction action = do
    var <- askDelegationState
    atomically $ do
        v0 <- readTVar var
        let (r,v1) = runState action v0
        -- The `$!` is necessary here, otherwise `runDelegationStateAction` will
        -- leak memory due to the fact `writeTVar` could end up accumulating thunks (if its
        -- content is never demanded). Note that using a strict State is not enough,
        -- as that ensure that only the sequencing of actions is strict, but the state
        -- `s` is still lazy, so is using `Control.Lens.%=`, which calls `modify` and
        -- not `modify'` under the hood.
        writeTVar var $! v1
        pure r

----------------------------------------------------------------------------
-- Common functions
----------------------------------------------------------------------------

-- | Retrieves last PSK in chain of delegation started by public key
-- and resolves the passed issuer to a public key. Uses database only.
getDlgTransPsk
    :: (MonadDBRead m, MonadUnliftIO m)
    => StakeholderId -> m (Maybe (PublicKey, ProxySKHeavy))
getDlgTransPsk issuer =
    runDBCede $ do
        lastPsk <- dlgLastPsk issuer
        firstPskIssuer <- fmap pskIssuerPk <$> getPsk issuer
        pure $ (,) <$> firstPskIssuer <*> lastPsk
