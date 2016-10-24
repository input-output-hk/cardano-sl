{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Storage with node local state which should be persistent.

module Pos.State.Storage
       (
         Storage

       , Query
       , getBlock
       , getHeadBlock
       , getLeaders
       , getLocalTxns
       , getOurCommitment
       , getOurOpening
       , getOurShares
       , mayBlockBeUseful

       , ProcessBlockRes (..)

       , Update
       , createNewBlock
       , setSecret
       , processBlock
       , processNewSlot
       , processCommitment
       , processOpening
       , processShares
       , processTx
       , processVssCertificate
       ) where

import           Control.Lens            (makeClassy, use, view, (.=), (^.))
import           Data.Acid               ()
import           Data.Default            (Default, def)
import           Data.List.NonEmpty      (NonEmpty ((:|)))
import           Data.SafeCopy           (base, deriveSafeCopySimple)
import           Serokell.AcidState      ()
import           Serokell.Util           (VerificationRes (..))
import           Universum

import           Pos.Crypto              (PublicKey, SecretKey, Share)
import           Pos.State.Storage.Block (BlockStorage, HasBlockStorage (blockStorage),
                                          blkCleanUp, blkCreateGenesisBlock,
                                          blkCreateNewBlock, blkProcessBlock, blkRollback,
                                          blkSetHead, getBlock, getHeadBlock, getLeaders,
                                          mayBlockBeUseful)
import           Pos.State.Storage.Mpc   (HasMpcStorage (mpcStorage), MpcStorage,
                                          getLocalMpcData, getOurCommitment,
                                          getOurOpening, getOurShares, mpcApplyBlocks,
                                          mpcProcessCommitment, mpcProcessOpening,
                                          mpcProcessShares, mpcProcessVssCertificate,
                                          mpcRollback, mpcVerifyBlock, mpcVerifyBlocks,
                                          setSecret)
import           Pos.State.Storage.Tx    (HasTxStorage (txStorage), TxStorage,
                                          getLocalTxns, processTx, txApplyBlocks,
                                          txRollback, txVerifyBlocks)
import           Pos.State.Storage.Types (AltChain, ProcessBlockRes (..), mkPBRabort)
import           Pos.Types               (Block, Commitment, CommitmentSignature,
                                          EpochIndex, MainBlock, Opening, SlotId (..),
                                          VssCertificate, blockTxs, epochIndexL,
                                          headerHashG, unflattenSlotId, verifyTxAlone)
import           Pos.Util                (readerToState, _neHead)

type Query  a = forall m . MonadReader Storage m => m a
type Update a = forall m . MonadState Storage m => m a

data Storage = Storage
    { -- | State of MPC.
      __mpcStorage   :: !MpcStorage
    , -- | Transactions part of /static-state/.
      __txStorage    :: !TxStorage
    , -- | Blockchain part of /static-state/.
      __blockStorage :: !BlockStorage
    , -- | Id of last seen slot.
      _slotId        :: !SlotId
    }

makeClassy ''Storage
deriveSafeCopySimple 0 'base ''Storage

instance HasMpcStorage Storage where
    mpcStorage = _mpcStorage
instance HasTxStorage Storage where
    txStorage = _txStorage
instance HasBlockStorage Storage where
    blockStorage = _blockStorage

instance Default Storage where
    def =
        Storage
        { __mpcStorage = def
        , __txStorage = def
        , __blockStorage = def
        , _slotId = unflattenSlotId 0
        }

getHeadEpoch :: Query EpochIndex
getHeadEpoch = view epochIndexL <$> getHeadBlock

-- | Create a new block on top of best chain.
createNewBlock :: SecretKey -> SlotId -> Update MainBlock
createNewBlock sk sId = do
    txs <- readerToState $ toList <$> getLocalTxns
    mpcData <- readerToState getLocalMpcData
    blk <- blkCreateNewBlock sk sId txs mpcData
    let blocks = Right blk :| []
    mpcApplyBlocks blocks
    blk <$ txApplyBlocks blocks

-- | Do all necessary changes when a block is received.
processBlock :: SlotId -> Block -> Update ProcessBlockRes
processBlock curSlotId blk = do
    mpcRes <- readerToState $ mpcVerifyBlock blk
    let txs =
            case blk of
                Left _        -> []
                Right mainBlk -> toList $ mainBlk ^. blockTxs
    let txRes = foldMap verifyTxAlone txs
    case mpcRes <> txRes of
        VerSuccess        -> processBlockDo curSlotId blk
        VerFailure errors -> return $ mkPBRabort errors

processBlockDo :: SlotId -> Block -> Update ProcessBlockRes
processBlockDo curSlotId blk = do
    r <- blkProcessBlock curSlotId blk
    case r of
        PBRgood (toRollback, chain) -> do
            mpcRes <- readerToState $ mpcVerifyBlocks toRollback chain
            txRes <- readerToState $ txVerifyBlocks toRollback chain
            case mpcRes <> txRes of
                VerSuccess        -> processBlockFinally toRollback chain
                VerFailure errors -> return $ mkPBRabort errors
        _ -> return r

-- At this point all checks have been passed and we know that we can
-- adopt this AltChain.
processBlockFinally :: Word -> AltChain -> Update ProcessBlockRes
processBlockFinally toRollback blocks = do
    mpcRollback toRollback
    mpcApplyBlocks blocks
    txRollback toRollback
    txApplyBlocks blocks
    blkRollback toRollback
    blkSetHead (blocks ^. _neHead . headerHashG)
    headEpoch <- readerToState getHeadEpoch
    knownEpoch <- use (slotId . epochIndexL)
    when (headEpoch + 1 == knownEpoch) $ createGenesisBlock knownEpoch
    return $ PBRgood (toRollback, blocks)

-- | Do all necessary changes when new slot starts.
processNewSlot :: SlotId -> Update ()
processNewSlot sId = do
    knownSlot <- use slotId
    when (sId > knownSlot) $ processNewSlotDo sId

processNewSlotDo :: SlotId -> Update ()
processNewSlotDo sId@SlotId {..} = do
    slotId .= sId
    when (siSlot == 0) $ createGenesisBlock siEpoch
    blkCleanUp sId

createGenesisBlock :: EpochIndex -> Update ()
createGenesisBlock epoch = do
    headEpoch <- readerToState getHeadEpoch
    knownEpoch <- use (slotId . epochIndexL)
    when (headEpoch + 1 == knownEpoch) $
        do leaders <- notImplemented
           () <$ blkCreateGenesisBlock epoch leaders

processCommitment :: PublicKey -> (Commitment, CommitmentSignature) -> Update ()
processCommitment = mpcProcessCommitment

processOpening :: PublicKey -> Opening -> Update ()
processOpening = mpcProcessOpening

processShares :: PublicKey -> HashMap PublicKey Share -> Update ()
processShares = mpcProcessShares

processVssCertificate :: PublicKey -> VssCertificate -> Update ()
processVssCertificate = mpcProcessVssCertificate

-- TODO: just use qualified imports for importing all that stuff from
-- Pos.State.Storage.Mpc and Pos.State.Storage.Block
