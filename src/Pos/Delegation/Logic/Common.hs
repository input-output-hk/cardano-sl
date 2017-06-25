{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Common methods/datatypes across Delegation.Logic.*

module Pos.Delegation.Logic.Common
       (
       -- * Exceptions
         DelegationError(..)

       -- * Modifying memstate
       , DelegationStateAction
       , runDelegationStateAction
       , invalidateProxyCaches

       -- * Common helpers
       , initDelegation
       , getPSKsFromThisEpoch
       , getDlgTransPsk
       ) where

import           Control.Exception         (Exception (..))
import           Control.Lens              ((%=), (.=))
import qualified Data.HashMap.Strict       as HM
import qualified Data.HashSet              as HS
import qualified Data.Text.Buildable       as B
import           Data.Time.Clock           (UTCTime, addUTCTime)
import           Formatting                (bprint, build, sformat, stext, (%))
import           Universum

import           Pos.Block.Core            (Block, mainBlockDlgPayload)
import           Pos.Constants             (lightDlgConfirmationTimeout,
                                            messageCacheTimeout)
import           Pos.Core                  (HeaderHash, epochIndexL, headerHash)
import           Pos.Crypto                (ProxySecretKey (..), PublicKey)
import           Pos.DB                    (DBError (DBMalformed), MonadDBRead)
import qualified Pos.DB.Block              as DB
import qualified Pos.DB.DB                 as DB
import           Pos.Delegation.Cede       (getPskChain, runDBCede)
import           Pos.Delegation.Class      (DelegationWrap (..), MonadDelegation,
                                            askDelegationState, dwConfirmationCache,
                                            dwEpochId, dwMessageCache, dwThisEpochPosted)
import           Pos.Delegation.DB         (getDlgTransitive)
import           Pos.Delegation.Types      (DlgPayload (getDlgPayload))
import           Pos.Exception             (cardanoExceptionFromException,
                                            cardanoExceptionToException)
import           Pos.Types                 (ProxySKHeavy, StakeholderId, addressHash)
import qualified Pos.Util.Concurrent.RWVar as RWV
import           Pos.Util.LRU              (filterLRU)

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
type DelegationStateAction m = StateT DelegationWrap m

-- Misha knows better probably.
{-# ANN runDelegationStateAction ("HLint: ignore Use fmap" :: Text) #-}
-- | Effectively takes a lock on ProxyCaches mvar in NodeContext and
-- allows you to run some computation producing updated ProxyCaches
-- and return value. Will put MVar back on exception.
runDelegationStateAction
    :: (MonadIO m, MonadMask m, MonadDelegation ctx m)
    => DelegationStateAction m a -> m a
runDelegationStateAction action = do
    var <- askDelegationState
    RWV.modify var $ \startState -> swap <$> runStateT action startState

-- | Invalidates proxy caches using built-in constants.
invalidateProxyCaches :: (Monad m) => UTCTime -> DelegationStateAction m ()
invalidateProxyCaches curTime = do
    dwMessageCache %=
        filterLRU (\t -> addUTCTime (toDiffTime messageCacheTimeout) t > curTime)
    dwConfirmationCache %=
        filterLRU (\t -> addUTCTime (toDiffTime lightDlgConfirmationTimeout) t > curTime)
  where
    toDiffTime (t :: Integer) = fromIntegral t

----------------------------------------------------------------------------
-- Common functions
----------------------------------------------------------------------------

-- | Retrieves psk certificated that have been accumulated before
-- given block. The block itself should be in DB.
getPSKsFromThisEpoch
    :: forall ssc m.
       DB.MonadBlockDB ssc m
    => HeaderHash -> m [ProxySKHeavy]
getPSKsFromThisEpoch tip =
    concatMap (either (const []) (getDlgPayload . view mainBlockDlgPayload)) <$>
        (DB.loadBlocksWhile @ssc) isRight tip

-- | Initializes delegation in-memory storage.
--
-- * Sets `_dwEpochId` to epoch of tip.
-- * Loads `_dwThisEpochPosted` from database
initDelegation
    :: forall ssc ctx m.
       (MonadIO m, DB.MonadBlockDB ssc m, MonadDelegation ctx m, MonadMask m)
    => m ()
initDelegation = do
    tip <- DB.getTipHeader @(Block ssc)
    let tipEpoch = tip ^. epochIndexL
    fromGenesisPsks <-
        map pskIssuerPk <$> (getPSKsFromThisEpoch @ssc) (headerHash tip)
    runDelegationStateAction $ do
        dwEpochId .= tipEpoch
        dwThisEpochPosted .= HS.fromList fromGenesisPsks

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
        let finalPsk = find (\psk -> pskDelegatePk psk == dPk) chain
        let iPk = pskIssuerPk <$>
                  find (\psk -> addressHash (pskIssuerPk psk) == issuer) chain
        let throwEmpty = throwM $ DBMalformed $
                sformat ("getDlgTransPk: couldn't find psk with dlgPk "%build%
                         " and issuer "%build)
                        dPk issuer
        maybe throwEmpty (pure . Just) $ (,) <$> iPk <*> finalPsk
