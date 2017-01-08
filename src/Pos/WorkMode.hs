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
       , TimedMode
       ) where


import           Control.Monad.Catch           (MonadMask)
import           Control.TimeWarp.Rpc          (Dialog, Transfer)
import           Control.TimeWarp.Timed        (MonadTimed (..), TimedIO)
import           System.Wlog                   (LoggerNameBox, WithLogger)
import           Universum

import           Pos.Communication.Types.State (MutSocketState)
import           Pos.Context                   (ContextHolder, WithNodeContext)
import qualified Pos.DB.Class                  as Modern
import qualified Pos.DB.Holder                 as Modern
import           Pos.Delegation.Class          (DelegationT (..), MonadDelegation)
import           Pos.DHT.Model                 (DHTPacking, MonadMessageDHT (..),
                                                WithDefaultMsgHeader)
import           Pos.DHT.Real                  (KademliaDHT (..))
import           Pos.Slotting                  (MonadSlots (..))
import           Pos.Ssc.Class.Helpers         (SscHelpersClass (..))
import           Pos.Ssc.Class.LocalData       (SscLocalDataClass)
import           Pos.Ssc.Class.Storage         (SscStorageClass)
import           Pos.Ssc.Extra                 (MonadSscGS, MonadSscLD, SscHolder)
import           Pos.Statistics.MonadStats     (MonadStats, NoStatsT, StatsT)
import           Pos.Txp.Class                 (MonadTxpLD (..))
import           Pos.Txp.Holder                (TxpLDHolder)
import           Pos.Types                     (MonadUtxo)
import           Pos.Update.Class              (MonadUS)
import           Pos.Update.Holder             (USHolder)
import           Pos.Util.JsonLog              (MonadJL (..))

type MSockSt ssc = MutSocketState ssc

-- | Bunch of constraints to perform work for real world distributed system.
type WorkMode ssc m
    = ( WithLogger m
      , MonadIO m
      , MonadFail m
      , MonadTimed m
      , MonadMask m
      , MonadSlots m
      , Modern.MonadDB ssc m
      , MonadTxpLD ssc m
      , MonadDelegation m
      , MonadUS m
      , MonadUtxo m
      , MonadSscGS ssc m
      , SscStorageClass ssc
      , SscLocalDataClass ssc
      , SscHelpersClass ssc
      , MonadSscLD ssc m
      , WithNodeContext ssc m
      , MonadMessageDHT (MSockSt ssc) m
      , WithDefaultMsgHeader m
      , MonadStats m
      , MonadJL m
      )

-- | More relaxed version of 'WorkMode'.
type MinWorkMode ss m
    = ( WithLogger m
      , MonadTimed m
      , MonadMask m
      , MonadIO m
      , MonadFail m
      , MonadMessageDHT ss m
      , WithDefaultMsgHeader m
      )

----------------------------------------------------------------------------
-- HZ
----------------------------------------------------------------------------

instance MonadJL m => MonadJL (KademliaDHT m) where
    jlLog = lift . jlLog

----------------------------------------------------------------------------
-- Concrete types
----------------------------------------------------------------------------

-- | RawRealMode is a basis for `WorkMode`s used to really run system.
type RawRealMode ssc =
    KademliaDHT (
    USHolder (
    DelegationT (
    TxpLDHolder ssc (
    SscHolder ssc (
    ContextHolder ssc (
    Modern.DBHolder ssc (
    Dialog DHTPacking (
    Transfer (
    MSockSt ssc)))))))))

-- | ProductionMode is an instance of WorkMode which is used
-- (unsurprisingly) in production.
type ProductionMode ssc = NoStatsT (RawRealMode ssc)

-- | StatsMode is used for remote benchmarking.
type StatsMode ssc = StatsT (RawRealMode ssc)

-- | ServiceMode is the mode in which support nodes work.
type ServiceMode = KademliaDHT (Dialog DHTPacking (Transfer ()))

-- | Mode in which time is abstracted and nothing else. Also logging
-- capabilities are provided.
type TimedMode = LoggerNameBox TimedIO
