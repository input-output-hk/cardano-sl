{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | All logic of certificates processing

module Pos.Delegation.Logic
       ( runDelegationStateM
       , invalidateProxyCaches
       , PSKVerdict (..)
       , processProxySecretKey
       , ConfirmPSKVerdict (..)
       , processConfirmProxySk
       ) where

import           Control.Concurrent.STM.TVar (readTVar, writeTVar)
import           Control.Lens                (to, (%=), (^.))
import qualified Data.HashMap.Strict         as HM
import           Data.Time.Clock             (UTCTime, addUTCTime, getCurrentTime)
import           Formatting                  (build, sformat, shown, (%))
import           System.Wlog                 (logDebug, logInfo)
import           Universum

import           Pos.Binary.Communication    ()
import           Pos.Communication.Methods   (sendProxyConfirmSK, sendProxySecretKey)
import           Pos.Communication.Types     (MutSocketState, ResponseMode)
import           Pos.Crypto                  (ProxySecretKey, checkProxySecretKey,
                                              pdDelegatePk, pdDelegatePk, proxySign,
                                              proxyVerify)
import           Pos.DB.Misc                 (addProxySecretKey, getProxySecretKeys)
import           Pos.Delegation.Class        (DelegationWrap, MonadDelegation (..),
                                              dwProxyConfCache, dwProxyMsgCache)
import           Pos.Delegation.Types        (CheckProxySKConfirmed (..),
                                              CheckProxySKConfirmedRes (..),
                                              ConfirmProxySK (..), SendProxySK (..))
import           Pos.Types                   (EpochIndex, ProxySKEpoch, ProxySigEpoch)
import           Pos.WorkMode                (WorkMode)


----------------------------------------------------------------------------
-- Different helpers to simplify logic
----------------------------------------------------------------------------

-- | Convenient monad to work in 'DelegationWrap' context while being
-- in STM. It represents actions/modifications that don't produce any
-- output.
newtype DelegationStateAction a = DelegationStateAction
    { getDelegationStateM :: StateT DelegationWrap STM a
    } deriving (Functor, Applicative, Monad, MonadState DelegationWrap)

-- | Effectively takes a lock on ProxyCaches mvar in NodeContext and
-- allows you to run some computation producing updated ProxyCaches
-- and return value. Will put MVar back on exception.
runDelegationStateM
    :: (MonadIO m, MonadDelegation m)
    => DelegationStateAction a -> m a
runDelegationStateM action = do
    var <- askDelegationState
    atomically $ do
        startState <- readTVar var
        (res,newState)<- runStateT (getDelegationStateM action) startState
        writeTVar var newState
        pure res

-- | Invalidates proxy caches using built-in constants.
invalidateProxyCaches :: UTCTime -> DelegationStateAction ()
invalidateProxyCaches curTime = do
    dwProxyMsgCache %= HM.filter (\t -> addUTCTime 60 t > curTime)
    dwProxyConfCache %= HM.filter (\t -> addUTCTime 500 t > curTime)


----------------------------------------------------------------------------
-- Proxy signing key propagation
----------------------------------------------------------------------------

-- | PSK check verdict. It can be unrelated (other key or spoiled, no
-- way to differ), exist in storage already or be cached.
data PSKVerdict
    = PSKUnrelated
    | PSKExists
    | PSKCached
    | PSKAdded
    deriving (Show)

-- | Processes proxy secret key (understands do we need it,
-- adds/caches on decision, returns this decision).
processProxySecretKey :: (MonadDelegation m, MonadIO m) => ProxySKEpoch -> m PSKVerdict
processProxySecretKey pSk = undefined
--    sk <- ncSecretKey <$> getNodeContext
--    pSks <- getProxySecretKeys
--    let related = checkProxySecretKey sk pSk
--        exists = pSk `elem` pSks
--        cached = p ^. ncProxyMsgCache . to (HM.member pSk)
--    if | cached -> pure (PSKCached, p)
--       | exists -> pure (PSKExists, p) -- cache here too?
--       | not (related) -> (PSKUnrelated,) <$> cachePSK p
--       | otherwise -> addProxySecretKey pSk $> (PSKAdded, p)
--  where
--    cachePSK p = do
--        curTime <- liftIO $ getCurrentTime
--        pure $ p & ncProxyMsgCache %~ HM.insert pSk curTime

----------------------------------------------------------------------------
-- PSK confirmation backpropagation
----------------------------------------------------------------------------

data ConfirmPSKVerdict
    = ConfirmPSKValid   -- ^ Valid, saved
    | ConfirmPSKInvalid -- ^ Invalid, throw away
    | ConfirmPSKCached  -- ^ Already saved
    deriving (Show)

processConfirmProxySk
    :: (WorkMode ssc m)
    => ProxySKEpoch
    -> ProxySigEpoch ProxySKEpoch
    -> m ConfirmPSKVerdict
processConfirmProxySk pSk proof = undefined
--    withProxyCaches $ \p -> do
--        let valid = proxyVerify (pdDelegatePk proof) proof (const True) pSk
--            cached = p ^. ncProxyConfCache . to (HM.member pSk)
--        if | cached -> pure (ConfirmPSKCached, p)
--           | not valid -> pure (ConfirmPSKInvalid, p)
--           | otherwise -> (ConfirmPSKValid,) <$> cachePsk p
--  where
--    cachePsk p = do
--        curTime <- liftIO $ getCurrentTime
--        pure $ p & ncProxyConfCache %~ HM.insert pSk curTime
