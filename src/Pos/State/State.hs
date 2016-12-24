{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | This module adds extra ecapsulation by hiding acid-state.

module Pos.State.State
       ( NodeState
       , MonadDB (getNodeState)
       , WorkModeDB
       , openState
       , openMemState
       , closeState

       -- * Simple getters.
       , getBlock
       , getBlockByDepth
       , getHeadBlock
       , getBestChain
       , getChainPart
       , getLeaders
       , getUtxo
       , getUtxoByDepth
       , getOldestUtxo
       , isTxVerified
       , getGlobalMpcData
       , getGlobalMpcDataByDepth
       , mayBlockBeUseful

       -- * Operations with effects.
       , ProcessBlockRes (..)
       , createNewBlock
       , processBlock
       , processNewSlot

       -- [CSL-103]: these function should be moved to GodTossing.
       -- * Functions for generating seed by SSC algorithm.
       , getParticipants

       -- * SscGodTossing simple getters and setters.
       , getOurShares
       ) where

import           Universum

import           Data.Default             (Default)
import           Data.List.NonEmpty       (NonEmpty)
import           Pos.DHT.Model            (DHTResponseT)
import           Pos.DHT.Real             (KademliaDHT)
import           Serokell.Util            (VerificationRes)
import           System.Wlog              (HasLoggerName, LogEvent, LoggerName,
                                           dispatchEvents, getLoggerName, logWarning,
                                           runPureLog, usingLoggerName)

import           Pos.Crypto               (ProxySecretKey, SecretKey, Share, VssKeyPair,
                                           VssPublicKey, decryptShare, toVssPublicKey)
import           Pos.Ssc.Class.Storage    (SscStorageClass (..))
import           Pos.Ssc.Class.Types      (Ssc (SscGlobalState, SscPayload, SscStorage))
import           Pos.State.Storage.Types  (ProcessBlockRes (..))
import           Pos.Statistics.StatEntry ()
import           Pos.Types                (Address, Block, EpochIndex, GenesisBlock,
                                           HeaderHash, IdTxWitness, MainBlock,
                                           MainBlockHeader, SlotId, SlotLeaders, Tx, TxId,
                                           TxWitness, Utxo)
import           Pos.Util                 (AsBinary, asBinary, fromBinaryM)

-- | NodeState encapsulates all the state stored by node.
class Monad m => MonadDB ssc m | m -> ssc where
    getNodeState :: m (NodeState ssc)

-- | Convenient type class to avoid passing NodeState throughout the code.
instance (Monad m, MonadDB ssc m) => MonadDB ssc (ReaderT r m) where
    getNodeState = lift getNodeState

instance (MonadDB ssc m, Monad m) => MonadDB ssc (StateT s m) where
    getNodeState = lift getNodeState

instance (Monad m, MonadDB ssc m) => MonadDB ssc (DHTResponseT s m) where
    getNodeState = lift getNodeState

instance (MonadDB ssc m, Monad m) => MonadDB ssc (KademliaDHT m) where
    getNodeState = lift getNodeState

-- | IO monad with db access.
type WorkModeDB ssc m = (MonadIO m, MonadDB ssc m)

type Storage ssc = ()

-- | State of the node.
type NodeState ssc = ()

type QUConstraint  ssc m = (WorkModeDB ssc m)
type QULConstraint ssc m = (WorkModeDB ssc m, HasLoggerName m)

-- | Open NodeState, reading existing state from disk (if any).
openState
    :: (Default (SscStorage ssc),
        MonadIO m)
    => Maybe (Storage ssc)
    -> Bool
    -> FilePath
    -> m (NodeState ssc)
openState = undefined

-- | Open NodeState which doesn't store anything on disk. Everything
-- is stored in memory and will be lost after shutdown.
openMemState
    :: (Default (SscStorage ssc),
        MonadIO m)
    => Maybe (Storage ssc)
    -> m (NodeState ssc)
openMemState = undefined

-- | Safely close NodeState.
closeState :: (MonadIO m, SscStorageClass ssc) => NodeState ssc -> m ()
closeState = const pass

-- | Get list of slot leaders for the given epoch. Empty list is returned
-- if no information is available.
getLeaders :: QUConstraint ssc m => EpochIndex -> m (Maybe SlotLeaders)
getLeaders = undefined

-- | Get Block by hash.
getBlock :: QUConstraint ssc m => HeaderHash ssc -> m (Maybe (Block ssc))
getBlock = undefined

-- | Get Block by depth
getBlockByDepth :: QUConstraint ssc m => Word -> m (Maybe (Block ssc))
getBlockByDepth = undefined

-- | Get block which is the head of the __best chain__.
getHeadBlock :: QUConstraint ssc m => m (Block ssc)
getHeadBlock = undefined

-- | Return current best chain.
getBestChain :: QUConstraint ssc m => m (NonEmpty (Block ssc))
getBestChain = undefined

-- | Return part of best chain with given limits
getChainPart :: QUConstraint ssc m
             => Maybe (HeaderHash ssc) -> Maybe (HeaderHash ssc) -> Maybe Word
             -> m (Either Text [Block ssc])
getChainPart = undefined

-- | Get Utxo by depth
getUtxoByDepth :: QUConstraint ssc m => Word -> m (Maybe Utxo)
getUtxoByDepth = undefined

-- | Get current Utxo
getUtxo :: QUConstraint ssc m => m Utxo
getUtxo = undefined

-- | Get oldest (genesis) utxo
getOldestUtxo :: QUConstraint ssc m => m Utxo
getOldestUtxo = undefined

-- | Checks if tx is verified
isTxVerified :: QUConstraint ssc m => (Tx, TxWitness) -> m Bool
isTxVerified = undefined

-- | Get global SSC data.
getGlobalMpcData :: QUConstraint ssc m => m (SscGlobalState ssc)
getGlobalMpcData = undefined

-- | Get global SSC data that was observed N blocks ago.
getGlobalMpcDataByDepth :: QUConstraint ssc m => Word ->  m (Maybe (SscGlobalState ssc))
getGlobalMpcDataByDepth = undefined

-- | Check that block header is correct and claims to represent block
-- which may become part of blockchain.
mayBlockBeUseful :: QUConstraint ssc m => SlotId -> MainBlockHeader ssc -> m VerificationRes
mayBlockBeUseful = undefined

-- | Create new block on top of currently known best chain, assuming
-- we are slot leader.
createNewBlock :: QUConstraint ssc m
               => [IdTxWitness]
               -> SecretKey
               -> Maybe (ProxySecretKey (EpochIndex,EpochIndex))
               -> SlotId
               -> SscPayload ssc
               -> m (Either Text (MainBlock ssc))
createNewBlock = undefined

-- | Notify NodeState about beginning of new slot. Ideally it should
-- be used before all other updates within this slot.
processNewSlot :: QULConstraint ssc m => SlotId -> m (Maybe (GenesisBlock ssc), [LogEvent])
processNewSlot = undefined

-- | Process some Block received from the network.
processBlock :: (QUConstraint ssc m)
             => [IdTxWitness]
             -> SlotId
             -> Block ssc
             -> m (ProcessBlockRes ssc)
processBlock = undefined

-- | Functions for generating seed by SSC algorithm
getParticipants
    :: QUConstraint ssc m
    => EpochIndex
    -> m (Maybe (NonEmpty (AsBinary VssPublicKey)))
getParticipants = undefined

----------------------------------------------------------------------------
-- Related to SscGodTossing
----------------------------------------------------------------------------

-- | Decrypt shares (in commitments) that are intended for us and that we can
-- decrypt.
getOurShares
    :: QULConstraint ssc m
    => VssKeyPair -> m (HashMap Address Share)
getOurShares = undefined
