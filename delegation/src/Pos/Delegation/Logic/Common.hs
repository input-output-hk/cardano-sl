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
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Buildable as B
import           Formatting (bprint, build, sformat, stext, (%))

import           Pos.Core (ProxySKHeavy, StakeholderId, addressHash)
import           Pos.Crypto (ProxySecretKey (..), PublicKey)
import           Pos.DB (DBError (DBMalformed), MonadDBRead)
import           Pos.Delegation.Cede (getPskChain, runDBCede)
import           Pos.Delegation.Class (DelegationWrap (..), MonadDelegation, askDelegationState)
import           Pos.Delegation.DB (getDlgTransitive)
import           Pos.Exception (cardanoExceptionFromException, cardanoExceptionToException)

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
    :: (MonadIO m, MonadMask m, MonadDelegation ctx m)
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
-- and resolves the passed issuer to a public key. Doesn't check that
-- user himself didn't delegate. Uses database only.
getDlgTransPsk
    :: MonadDBRead m
    => StakeholderId -> m (Maybe (PublicKey, ProxySKHeavy))
getDlgTransPsk issuer = getDlgTransitive issuer >>= \case
    Nothing -> pure Nothing
    Just dPk -> do
        chain <- runDBCede $ HM.elems <$> getPskChain issuer
        let finalPsk =
                find (\psk -> addressHash (pskDelegatePk psk) == dPk) chain
        let iPk = pskIssuerPk <$>
                  find (\psk -> addressHash (pskIssuerPk psk) == issuer) chain
        let throwEmpty = throwM $ DBMalformed $
                sformat ("getDlgTransPk: couldn't find psk with dlgPk "%build%
                         " and issuer "%build)
                        dPk issuer
        maybe throwEmpty (pure . Just) $ (,) <$> iPk <*> finalPsk
