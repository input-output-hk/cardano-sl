{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-| 'WorkMode' constraint. It is widely used in almost every our code.
    Simple alias for bunch of useful constraints. This module also
    contains new monads to extend functional capabilities inside do-block.
-}

module Pos.WorkMode
       ( WorkMode
       , MinWorkMode

       -- * Actual modes
       , ProductionMode
       , RawRealMode
       , ServiceMode
       , StatsMode
       ) where


import           Control.Monad.Catch         (MonadMask)
import           Mockable                    (MonadMockable)
import           Mockable.Production         (Production)
import           System.Wlog                 (LoggerNameBox (..), WithLogger)
import           Universum

import           Pos.Communication.PeerState (PeerStateHolder (..), WithPeerState)
import           Pos.Context                 (ContextHolder, WithNodeContext)
import           Pos.DB.Class                (MonadDB)
import           Pos.DB.Holder               (DBHolder)
import           Pos.Delegation.Class        (MonadDelegation)
import           Pos.Delegation.Holder       (DelegationT (..))
import           Pos.DHT.Model               (MonadDHT)
import           Pos.DHT.Real                (KademliaDHT (..), WithKademliaDHTInstance)
import           Pos.Slotting                (MonadSlots (..))
import           Pos.Ssc.Class.Helpers       (SscHelpersClass (..))
import           Pos.Ssc.Class.LocalData     (SscLocalDataClass)
import           Pos.Ssc.Class.Storage       (SscStorageClass)
import           Pos.Ssc.Extra               (MonadSscGS, MonadSscLD, SscHolder)
import           Pos.Statistics.MonadStats   (MonadStats, NoStatsT, StatsT)
import           Pos.Txp.Class               (MonadTxpLD (..))
import           Pos.Txp.Holder              (TxpLDHolder)
import           Pos.Types                   (MonadUtxo, MonadUtxoRead)
import           Pos.Update.MemState         (MonadUSMem, USHolder)
import           Pos.Util.JsonLog            (MonadJL (..))

-- | Bunch of constraints to perform work for real world distributed system.
type WorkMode ssc m
    = ( MinWorkMode m
      , MonadMask m
      , MonadSlots m
      , MonadDB ssc m
      , MonadTxpLD ssc m
      , MonadDelegation m
      , MonadUtxo m
      , MonadSscGS ssc m
      , SscStorageClass ssc
      , SscLocalDataClass ssc
      , SscHelpersClass ssc
      , MonadSscLD ssc m
      , WithNodeContext ssc m
      , MonadStats m
      , MonadJL m
      , WithKademliaDHTInstance m
      , WithPeerState ssc m
      , MonadUSMem m
      )

-- | More relaxed version of 'WorkMode'.
type MinWorkMode m
    = ( WithLogger m
      , MonadMockable m
      , MonadDHT m
      , MonadIO m
      )

----------------------------------------------------------------------------
-- HZ
----------------------------------------------------------------------------

instance MonadJL m => MonadJL (KademliaDHT m) where
    jlLog = lift . jlLog

----------------------------------------------------------------------------
-- Concrete types
----------------------------------------------------------------------------

-- Maybe we should move to somewhere else
deriving instance MonadUtxoRead m => MonadUtxoRead (KademliaDHT m)
deriving instance MonadUtxo m => MonadUtxo (KademliaDHT m)
deriving instance (Monad m, WithNodeContext ssc m) => WithNodeContext ssc (KademliaDHT m)
deriving instance MonadDB ssc m => MonadDB ssc (KademliaDHT m)
deriving instance MonadSscGS ssc m => MonadSscGS ssc (KademliaDHT m)
deriving instance MonadDelegation m => MonadDelegation (KademliaDHT m)
deriving instance MonadUSMem m => MonadUSMem (KademliaDHT m)

deriving instance MonadSscLD ssc m => MonadSscLD ssc (PeerStateHolder ssc m)
deriving instance MonadUtxoRead m => MonadUtxoRead (PeerStateHolder ssc m)
deriving instance MonadUtxo m => MonadUtxo (PeerStateHolder ssc m)
deriving instance (Monad m, WithNodeContext ssc m) => WithNodeContext ssc (PeerStateHolder ssc m)
deriving instance MonadDB ssc m => MonadDB ssc (PeerStateHolder ssc m)
deriving instance MonadSlots m => MonadSlots (PeerStateHolder ssc m)
deriving instance MonadDHT m => MonadDHT (PeerStateHolder ssc m)
deriving instance MonadSscGS ssc m => MonadSscGS ssc (PeerStateHolder ssc m)
deriving instance MonadDelegation m => MonadDelegation (PeerStateHolder ssc m)
deriving instance MonadTxpLD ssc m => MonadTxpLD ssc (PeerStateHolder ssc m)
deriving instance MonadJL m => MonadJL (PeerStateHolder ssc m)
deriving instance MonadUSMem m => MonadUSMem (PeerStateHolder ssc m)
deriving instance (Monad m, WithKademliaDHTInstance m) => WithKademliaDHTInstance (PeerStateHolder ssc m)

-- | RawRealMode is a basis for `WorkMode`s used to really run system.
type RawRealMode ssc =
    PeerStateHolder ssc (
    KademliaDHT (
    USHolder (
    DelegationT (
    TxpLDHolder ssc (
    SscHolder ssc (
    ContextHolder ssc (
    DBHolder ssc (
    LoggerNameBox Production
    ))))))))

-- | ProductionMode is an instance of WorkMode which is used
-- (unsurprisingly) in production.
type ProductionMode ssc = NoStatsT (RawRealMode ssc)

-- | StatsMode is used for remote benchmarking.
type StatsMode ssc = StatsT (RawRealMode ssc)

-- | ServiceMode is the mode in which support nodes work.
type ServiceMode = KademliaDHT (LoggerNameBox Production)
