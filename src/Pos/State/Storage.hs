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
       , getLocalTxs
       , getOurCommitment
       , getOurOpening
       , getOurShares
       , getParticipants
       , getThreshold
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

       , IdTimestamp (..)
       , addStatRecord
       , getStatRecords
       ) where

import           Control.Lens            (makeClassy, use, view, (.=), (^.))
import           Data.Acid               ()
import           Data.Default            (Default, def)
import qualified Data.HashMap.Strict     as HM
import           Data.List               (nub)
import           Data.List.NonEmpty      (NonEmpty ((:|)))
import           Data.SafeCopy           (base, deriveSafeCopySimple)
import           Formatting              (build, sformat, (%))
import           Serokell.AcidState      ()
import           Serokell.Util           (VerificationRes (..))
import           Universum

import           Pos.Constants           (k)
import           Pos.Crypto              (PublicKey, SecretKey, Share, Threshold,
                                          VssPublicKey, signedValue)
import           Pos.State.Storage.Block (BlockStorage, HasBlockStorage (blockStorage),
                                          blkCleanUp, blkCreateGenesisBlock,
                                          blkCreateNewBlock, blkProcessBlock, blkRollback,
                                          blkSetHead, getBlock, getHeadBlock, getLeaders,
                                          getSlotDepth, mayBlockBeUseful)
import           Pos.State.Storage.Mpc   (HasMpcStorage (mpcStorage), MpcStorage,
                                          calculateLeaders, getGlobalMpcDataByDepth,
                                          getLocalMpcData, getOurCommitment,
                                          getOurOpening, getOurShares, mpcApplyBlocks,
                                          mpcProcessCommitment, mpcProcessOpening,
                                          mpcProcessShares, mpcProcessVssCertificate,
                                          mpcRollback, mpcVerifyBlock, mpcVerifyBlocks,
                                          setSecret)
import           Pos.State.Storage.Stats (HasStatsData (statsData), IdTimestamp (..),
                                          StatsData, addStatRecord, getStatRecords)
import           Pos.State.Storage.Tx    (HasTxStorage (txStorage), TxStorage,
                                          getLocalTxs, getUtxoByDepth, processTx,
                                          txApplyBlocks, txRollback, txVerifyBlocks)
import           Pos.State.Storage.Types (AltChain, ProcessBlockRes (..), mkPBRabort)
import           Pos.Types               (Block, Commitment, CommitmentSignature,
                                          EpochIndex, GenesisBlock, MainBlock, Opening,
                                          SlotId (..), SlotLeaders, VssCertificate,
                                          blockTxs, epochIndexL, getAddress, headerHashG,
                                          mdVssCertificates, txOutAddress,
                                          unflattenSlotId, verifyTxAlone)
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
    , -- | Statistical data
      __statsData    :: !StatsData
    }

makeClassy ''Storage
deriveSafeCopySimple 0 'base ''Storage

instance HasMpcStorage Storage where
    mpcStorage = _mpcStorage
instance HasTxStorage Storage where
    txStorage = _txStorage
instance HasBlockStorage Storage where
    blockStorage = _blockStorage
instance HasStatsData Storage where
    statsData = _statsData

instance Default Storage where
    def =
        Storage
        { __mpcStorage = def
        , __txStorage = def
        , __blockStorage = def
        , _slotId = unflattenSlotId 0
        , __statsData = def
        }

getHeadEpoch :: Query EpochIndex
getHeadEpoch = view epochIndexL <$> getHeadBlock

-- | Create a new block on top of best chain.
createNewBlock :: SecretKey -> SlotId -> Update MainBlock
createNewBlock sk sId = do
    txs <- readerToState $ toList <$> getLocalTxs
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
    knownEpoch <- use (slotId . epochIndexL)
    -- When we adopt alternative chain, it may revert genesis block
    -- already created for current epoch. And we will be in situation
    -- where best chain doesn't have genesis block for current epoch.
    -- If then we need to create block in current epoch, it will be
    -- definitely invalid. To prevent it we create genesis block after
    -- possible revert. Note that createGenesisBlock function will
    -- create block only for epoch which is one more than epoch of
    -- head, so we don't perform such check here.  Also note that it
    -- won't be necessary after we introduce `canCreateBlock` (or
    -- maybe we already did and this comment is outdated then), but it
    -- still will be good as an optimization. Even if later we see
    -- that there were other valid blocks in old epoch, we will
    -- replace chain and everything will be fine.
    createGenesisBlock knownEpoch $> ()
    return $ PBRgood (toRollback, blocks)

-- | Do all necessary changes when new slot starts.
processNewSlot :: SlotId -> Update ()
processNewSlot sId = do
    knownSlot <- use slotId
    when (sId > knownSlot) $ processNewSlotDo sId

processNewSlotDo :: SlotId -> Update ()
processNewSlotDo sId@SlotId {..} = do
    slotId .= sId
    when (siSlot == 0) $
        createGenesisBlock siEpoch >>=
        maybe (pure ()) (mpcApplyBlocks . pure . Left)
    blkCleanUp sId

createGenesisBlock :: EpochIndex -> Update (Maybe GenesisBlock)
createGenesisBlock epoch = do
    headEpoch <- readerToState getHeadEpoch
    if (headEpoch + 1 == epoch)
        then do
            leaders <- readerToState $ calculateLeadersDo epoch
            Just <$> blkCreateGenesisBlock epoch leaders
        else return Nothing

calculateLeadersDo :: EpochIndex -> Query SlotLeaders
calculateLeadersDo epoch = do
    depth <- getSlotDepth $ mpcCrucialSlot epoch
    utxo <- fromMaybe onErrorGetUtxo <$> getUtxoByDepth depth
    -- TODO: overall 'calculateLeadersDo' gets utxo twice, could be optimised
    threshold <- getThreshold epoch
    either onErrorCalcLeaders identity <$> calculateLeaders utxo threshold
  where
    onErrorGetUtxo =
        panic "Failed to get utxo necessary for leaders calculation"
    onErrorCalcLeaders e =
        panic (sformat ("Leaders calculation reported error: " % build) e)

-- | Get keys of nodes participating in an epoch. A node participates if,
-- when there were 'k' slots left before the end of the previous epoch, both
-- of these were true:
--
--   1. it was a stakeholder
--   2. it had already sent us its VSS key by that time
getParticipants :: EpochIndex -> Query [VssPublicKey]
getParticipants epoch = do
    depth <- getSlotDepth $ mpcCrucialSlot epoch
    utxo <- fromMaybe onErrorGetUtxo <$> getUtxoByDepth depth
    keymap <- maybe onErrorGetKeymap (view mdVssCertificates) <$>
              getGlobalMpcDataByDepth depth
    let stakeholders = nub $ map (getAddress . txOutAddress) (toList utxo)
    return $ map signedValue $ mapMaybe (`HM.lookup` keymap) stakeholders
  where
    onErrorGetUtxo =
        panic "Failed to get utxo necessary to enumerate participants"
    onErrorGetKeymap =
        panic "Failed to get certificates necessary to enumerate participants"

-- slot such that data after it is used for MPC in given epoch
mpcCrucialSlot :: EpochIndex -> SlotId
mpcCrucialSlot 0     = SlotId {siEpoch = 0, siSlot = 0}
mpcCrucialSlot epoch = SlotId {siEpoch = epoch - 1, siSlot = 5 * k - 1}

getThreshold :: EpochIndex -> Query Threshold
getThreshold epoch = do
    ps <- getParticipants epoch
    let len = length ps
    return (toInteger (len `div` 2 + len `mod` 2))

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
