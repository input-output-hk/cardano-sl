{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | All logic of certificates processing

module Pos.Delegation.Logic
       (
       -- * Helpers
         DelegationStateAction(..)
       , runDelegationStateAction
       , invalidateProxyCaches

       -- * Heavyweight psks handling
       , PskSimpleVerdict (..)
       , processProxySKSimple
       , delegationApplyBlocks
       , delegationVerifyBlocks
       , delegationRollbackBlocks

       -- * Lightweight psks handling
       , PskEpochVerdict (..)
       , processProxySKEpoch

       -- * Confirmations
       , ConfirmPskEpochVerdict (..)
       , processConfirmProxySk
       , isProxySKConfirmed
       ) where

import           Control.Concurrent.STM.TVar (readTVar, writeTVar)
import           Control.Lens                (uses, (%=))
import qualified Data.HashMap.Strict         as HM
import           Data.List.NonEmpty          (NonEmpty)
import           Data.Time.Clock             (UTCTime, addUTCTime, getCurrentTime)
import           Database.RocksDB            (BatchOp)
import           System.Wlog                 (WithLogger)
import           Universum

import           Pos.Binary.Communication    ()
import           Pos.Context                 (WithNodeContext (getNodeContext),
                                              ncSecretKey)
import           Pos.Crypto                  (pdDelegatePk, proxyVerify, pskDelegatePk,
                                              toPublic, verifyProxySecretKey)
import           Pos.DB.Class                (MonadDB)
import           Pos.DB.Misc                 (addProxySecretKey, getProxySecretKeys)
import           Pos.Delegation.Class        (DelegationWrap, MonadDelegation (..),
                                              dwProxyConfCache, dwProxyMsgCache)
import           Pos.Delegation.Types        (SendProxySK (..))
import           Pos.Types                   (Block, Blund, NEBlocks, ProxySKEpoch,
                                              ProxySKSimple, ProxySigEpoch, Undo)


----------------------------------------------------------------------------
-- Different helpers to simplify logic
----------------------------------------------------------------------------

-- | Convenient monad to work in 'DelegationWrap' context while being
-- in STM.
newtype DelegationStateAction a = DelegationStateAction
    { getDelegationStateM :: StateT DelegationWrap STM a
    } deriving (Functor, Applicative, Monad, MonadState DelegationWrap)

-- | Effectively takes a lock on ProxyCaches mvar in NodeContext and
-- allows you to run some computation producing updated ProxyCaches
-- and return value. Will put MVar back on exception.
runDelegationStateAction
    :: (MonadIO m, MonadDelegation m)
    => DelegationStateAction a -> m a
runDelegationStateAction action = do
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

type DelegationWorkMode ssc m = (MonadDelegation m, MonadDB ssc m, WithLogger m)

----------------------------------------------------------------------------
-- Heavyweight PSK
----------------------------------------------------------------------------

-- | Datatypes representing a verdict of simple PSK processing.
data PskSimpleVerdict
    = PSExists
    | PSCached
    | PSAdded
    deriving (Show,Eq)

processProxySKSimple
    :: (DelegationWorkMode ssc m)
    => ProxySKSimple -> m PskSimpleVerdict
processProxySKSimple = notImplemented

delegationApplyBlocks
    :: (DelegationWorkMode ssc m)
    => NonEmpty (Blund ssc) -> m (NonEmpty [BatchOp])
delegationApplyBlocks = notImplemented

-- | Verifies if blocks are correct relatively to the delegation logic
-- an returns non-empty list of proxySKs needed for undoing them.
delegationVerifyBlocks
    :: (MonadDB ssc m)
    => NEBlocks ssc -> m (Either Text (NonEmpty [ProxySKSimple]))
delegationVerifyBlocks = notImplemented

delegationRollbackBlocks
    :: (WithLogger m, MonadDB ssc m)
    => NonEmpty (Block ssc, Undo) -> m ()
delegationRollbackBlocks = notImplemented

----------------------------------------------------------------------------
-- Lightweight PSK propagation
----------------------------------------------------------------------------

-- | PSK check verdict. It can be unrelated (other key or spoiled, no
-- way to differ), exist in storage already or be cached.
data PskEpochVerdict
    = PEUnrelated
    | PEInvalid
    | PEExists
    | PECached
    | PEAdded
    deriving (Show,Eq)

-- TODO Calls to DB are not synchronized for now, because storage is
-- append-only, so nothing bad should happen. But it may be a problem
-- later.
-- | Processes proxy secret key (understands do we need it,
-- adds/caches on decision, returns this decision).
processProxySKEpoch
    :: (MonadDelegation m, WithNodeContext ssc m, MonadDB ssc m)
    => ProxySKEpoch -> m PskEpochVerdict
processProxySKEpoch pSk = do
    sk <- ncSecretKey <$> getNodeContext
    curTime <- liftIO getCurrentTime
    -- (1) We're reading from DB
    pSks <- getProxySecretKeys
    res <- runDelegationStateAction $ do
        let related = toPublic sk == pskDelegatePk pSk
            exists = pSk `elem` pSks
            msg = SendProxySKEpoch pSk
            valid = verifyProxySecretKey pSk
        cached <- uses dwProxyMsgCache $ HM.member msg
        dwProxyMsgCache %= HM.insert msg curTime
        pure $ if | not valid -> PEInvalid
                  | cached -> PECached
                  | exists -> PEExists
                  | not related -> PEUnrelated
                  | otherwise -> PEAdded
    -- (2) We're writing to DB
    when (res == PEAdded) $ addProxySecretKey pSk
    pure res

----------------------------------------------------------------------------
-- Lightweight PSK confirmation backpropagation
----------------------------------------------------------------------------

-- | Verdict of 'processConfirmProxySk' function
data ConfirmPskEpochVerdict
    = CPValid   -- ^ Valid, saved
    | CPInvalid -- ^ Invalid, throw away
    | CPCached  -- ^ Already saved
    deriving (Show,Eq)

-- | Takes a lightweight psk, delegate proof of delivery. Checks if
-- it's valid or not. Caches message in any case.
processConfirmProxySk
    :: (MonadDelegation m, MonadIO m)
    => ProxySKEpoch -> ProxySigEpoch ProxySKEpoch -> m ConfirmPskEpochVerdict
processConfirmProxySk pSk proof = do
    curTime <- liftIO getCurrentTime
    runDelegationStateAction $ do
        let valid = proxyVerify (pdDelegatePk proof) proof (const True) pSk
        cached <- uses dwProxyConfCache $ HM.member pSk
        when valid $ dwProxyConfCache %= HM.insert pSk curTime
        pure $ if | cached -> CPCached
                  | not valid -> CPInvalid
                  | otherwise -> CPValid

-- | Checks if we hold a confirmation for given PSK.
isProxySKConfirmed :: ProxySKEpoch -> DelegationStateAction Bool
isProxySKConfirmed pSk = uses dwProxyConfCache $ HM.member pSk
