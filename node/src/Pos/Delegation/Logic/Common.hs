{-# LANGUAGE AllowAmbiguousTypes #-}

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
       , mkDelegationVar
       , getDlgTransPsk
       ) where

import           Universum

import           Control.Exception    (Exception (..))
import           Control.Lens         ((%=))
import qualified Data.Cache.LRU       as LRU
import qualified Data.HashMap.Strict  as HM
import qualified Data.Text.Buildable  as B
import           Data.Time.Clock      (UTCTime, addUTCTime)
import           Formatting           (bprint, build, sformat, stext, (%))

import           Pos.Configuration    (dlgCacheParam, lightDlgConfirmationTimeout,
                                       messageCacheTimeout, HasNodeConfiguration)
import           Pos.Core             (HasConfiguration, ProxySKHeavy, StakeholderId,
                                       addressHash, headerHash)
import           Pos.Crypto           (ProxySecretKey (..), PublicKey)
import           Pos.DB               (DBError (DBMalformed), MonadDBRead)
import qualified Pos.DB.Block         as DB
import qualified Pos.DB.DB            as DB
import           Pos.Delegation.Cede  (getPskChain, runDBCede)
import           Pos.Delegation.Class (DelegationVar, DelegationWrap (..),
                                       MonadDelegation, askDelegationState,
                                       dwConfirmationCache, dwMessageCache)
import           Pos.Delegation.DB    (getDlgTransitive)
import           Pos.Exception        (cardanoExceptionFromException,
                                       cardanoExceptionToException)
import           Pos.Util.LRU         (filterLRU)

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
        writeTVar var v1
        pure r

-- | Invalidates proxy caches using built-in constants.
invalidateProxyCaches :: HasNodeConfiguration => UTCTime -> DelegationStateAction ()
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

-- | Make a new 'DelegationVar' and initialize it.
--
-- * Sets '_dwEpochId' to epoch of tip.
-- * Initializes mempools/LRU caches.
mkDelegationVar ::
       forall ssc m. (MonadIO m, DB.MonadBlockDB ssc m, HasConfiguration, HasNodeConfiguration)
    => m DelegationVar
mkDelegationVar = do
    tip <- DB.getTipHeader @ssc
    newTVarIO
        DelegationWrap
        { _dwMessageCache = LRU.newLRU msgCacheLimit
        , _dwConfirmationCache = LRU.newLRU confCacheLimit
        , _dwProxySKPool = HM.empty
        , _dwPoolSize = 1 -- approximate size of the empty mempool.
        , _dwTip = headerHash tip
        }
  where
    msgCacheLimit = Just dlgCacheParam
    confCacheLimit = Just (dlgCacheParam `div` 5)

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
