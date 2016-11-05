{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Storage with node local state which should be persistent.

module Pos.State.Storage
       (
         Storage
       , storageFromUtxo

       , Query
       , getBlock
       , getHeadBlock
       , getGlobalSscPayload
       , getLeaders
       , getLocalTxs
       , getLocalSscPayload
       , getOurShares
       , getParticipants
       , getThreshold
       , getToken
       , mayBlockBeUseful

       , ProcessBlockRes (..)
       , ProcessTxRes (..)

       , Update
       , createNewBlock
       , processBlock
       , processNewSlot
       , processSscMessage
       , processTx
       , setToken

       , IdTimestamp (..)
       , addStatRecord
       , getStatRecords
       ) where

import           Control.Lens            (makeClassy, use, view, (.=), (^.))
import           Control.Monad.TM        ((.=<<.))
import           Data.Acid               ()
import           Data.Default            (Default, def)
import qualified Data.HashMap.Strict     as HM
import           Data.List               (nub)
import           Data.List.NonEmpty      (NonEmpty ((:|)), nonEmpty)
import           Data.SafeCopy           (base, deriveSafeCopySimple)
import           Formatting              (build, sformat, (%))
import           Serokell.AcidState      ()
import           Serokell.Util           (VerificationRes (..), verifyGeneric)
import           Universum

import           Pos.Constants           (k)
import           Pos.Crypto              (PublicKey, SecretKey, Share, Threshold,
                                          VssKeyPair, VssPublicKey, signedValue)
import           Pos.Ssc.Class.Storage   (HasSscStorage (..), SscStorageClass (..))
import           Pos.Ssc.Class.Types     (SscTypes (..))
import           Pos.Ssc.DynamicState    (DSPayload (..), SscDynamicState, isCommitmentId,
                                          isOpeningId, isSharesId, _mdVssCertificates)
import           Pos.State.Storage.Block (BlockStorage, HasBlockStorage (blockStorage),
                                          blkCleanUp, blkCreateGenesisBlock,
                                          blkCreateNewBlock, blkProcessBlock, blkRollback,
                                          blkSetHead, getBlock, getHeadBlock, getLeaders,
                                          getSlotDepth, mayBlockBeUseful)
import qualified Pos.State.Storage.Mpc   as Mpc (calculateLeaders)
import           Pos.State.Storage.Stats (HasStatsData (statsData), IdTimestamp (..),
                                          StatsData, addStatRecord, getStatRecords)
import           Pos.State.Storage.Tx    (HasTxStorage (txStorage), TxStorage,
                                          getLocalTxs, getUtxoByDepth, processTx,
                                          txApplyBlocks, txRollback, txStorageFromUtxo,
                                          txVerifyBlocks)
import           Pos.State.Storage.Types (AltChain, ProcessBlockRes (..),
                                          ProcessTxRes (..), mkPBRabort)
import           Pos.Types               (Block, EpochIndex, GenesisBlock, MainBlock,
                                          SlotId (..), SlotLeaders, Utxo, blockMpc,
                                          blockSlot, blockTxs, epochIndexL, flattenSlotId,
                                          getAddress, headerHashG, txOutAddress,
                                          unflattenSlotId, verifyTxAlone)
import           Pos.Util                (readerToState, _neLast)

type Query  a = forall m. MonadReader Storage m => m a
type Update a = forall m. MonadState Storage m => m a

data Storage = Storage
    { -- | State of MPC.
      __mpcStorage   :: !(SscStorage SscDynamicState)
    , -- | Transactions part of /static-state/.
      __txStorage    :: !TxStorage
    , -- | Blockchain part of /static-state/.
      __blockStorage :: !(BlockStorage SscDynamicState)
    , -- | Id of last seen slot.
      _slotId        :: !SlotId
    , -- | Statistical data
      __statsData    :: !StatsData
    }

makeClassy ''Storage
deriveSafeCopySimple 0 'base ''Storage

instance (ssc ~ SscDynamicState) => HasSscStorage ssc Storage where
    sscStorage = _mpcStorage
instance HasTxStorage Storage where
    txStorage = _txStorage
instance HasBlockStorage Storage SscDynamicState where
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

-- | Create default storage with specified utxo
storageFromUtxo :: Utxo -> Storage
storageFromUtxo u = Storage def (txStorageFromUtxo u) def (unflattenSlotId 0) def

getHeadSlot :: Query (Either EpochIndex SlotId)
getHeadSlot = bimap (view epochIndexL) (view blockSlot) <$> getHeadBlock

getLocalSscPayload :: Query DSPayload
getLocalSscPayload = sscGetLocalPayload

getGlobalSscPayload :: Query DSPayload
getGlobalSscPayload = sscGetGlobalPayload

getToken :: Query (Maybe (SscToken SscDynamicState))
getToken = sscGetToken

getOurShares
    :: VssKeyPair -- ^ Our VSS key
    -> Integer -- ^ Random generator seed (needed for 'decryptShare')
    -> Query (HashMap PublicKey Share)
getOurShares = sscGetOurShares

-- | Create a new block on top of best chain if possible.
-- Block can be created if:
-- • we know genesis block for epoch from given SlotId
-- • last known block is not more than k slots away from
-- given SlotId
createNewBlock
    :: SecretKey -> SlotId -> Update (Maybe (MainBlock SscDynamicState))
createNewBlock sk sId = do
    ifM (readerToState (canCreateBlock sId))
        (Just <$> createNewBlockDo sk sId)
        (pure Nothing)

createNewBlockDo :: SecretKey -> SlotId -> Update (MainBlock SscDynamicState)
createNewBlockDo sk sId = do
    txs <- readerToState $ toList <$> getLocalTxs
    mpcData <- readerToState sscGetLocalPayload
    blk <- blkCreateNewBlock sk sId txs mpcData
    let blocks = Right blk :| []
    sscApplyBlocks blocks
    blk <$ txApplyBlocks blocks

canCreateBlock :: SlotId -> Query Bool
canCreateBlock sId = do
    maxSlotId <- canCreateBlockMax
    --identity $! traceM $ "[~~~~~~] canCreateBlock: slotId=" <> pretty slotId <> " < max=" <> pretty max <> " = " <> show (flattenSlotId slotId < flattenSlotId max)
    return (sId <= maxSlotId)
  where
    canCreateBlockMax = addKSafe . either (`SlotId` 0) identity <$> getHeadSlot
    addKSafe si = si {siSlot = min (6 * k - 1) (siSlot si + k)}

-- | Do all necessary changes when a block is received.
processBlock :: SlotId
             -> Block SscDynamicState
             -> Update (ProcessBlockRes SscDynamicState)
processBlock curSlotId blk = do
    -- TODO: I guess these checks should be part of block verification actually.
    let verifyMpc mainBlk =
            verifyMpcData (mainBlk ^. blockSlot) (mainBlk ^. blockMpc)
    let mpcRes = either (const mempty) verifyMpc blk
    let txs =
            case blk of
                Left _        -> []
                Right mainBlk -> toList $ mainBlk ^. blockTxs
    let txRes = foldMap verifyTxAlone txs
    case mpcRes <> txRes of
        VerSuccess        -> processBlockDo curSlotId blk
        VerFailure errors -> return $ mkPBRabort errors

-- | Verify MpcData using limited data.
-- TODO: more checks.
-- TODO: move this somewhere more appropriate
verifyMpcData :: SlotId -> DSPayload -> VerificationRes
verifyMpcData slotId DSPayload {..} =
    verifyGeneric
        [ ( null _mdCommitments || isCommitmentId slotId
          , "there are commitments in inappropriate block")
        , ( null _mdOpenings || isOpeningId slotId
          , "there are openings in inappropriate block")
        , ( null _mdShares || isSharesId slotId
          , "there are shares in inappropriate block")
        ]

processBlockDo :: SlotId
               -> Block SscDynamicState
               -> Update (ProcessBlockRes SscDynamicState)
processBlockDo curSlotId blk = do
    r <- blkProcessBlock curSlotId blk
    case r of
        PBRgood (toRollback, chain) -> do
            mpcRes <- readerToState $ sscVerifyBlocks toRollback chain
            txRes <- readerToState $ txVerifyBlocks toRollback chain
            case mpcRes <> txRes of
                VerSuccess        -> processBlockFinally toRollback chain
                VerFailure errors -> return $ mkPBRabort errors
        -- if we need block which we already know, we just use it
        PBRmore h ->
            maybe (pure r) (processBlockDo curSlotId) =<<
            readerToState (getBlock h)
        _ -> return r

-- At this point all checks have been passed and we know that we can
-- adopt this AltChain.
processBlockFinally :: Word
                    -> AltChain SscDynamicState
                    -> Update (ProcessBlockRes SscDynamicState)
processBlockFinally toRollback blocks = do
    sscRollback toRollback
    sscApplyBlocks blocks
    txRollback toRollback
    txApplyBlocks blocks
    blkRollback toRollback
    blkSetHead (blocks ^. _neLast . headerHashG)
    knownEpoch <- use (slotId . epochIndexL)
    -- When we adopt alternative chain, it may revert genesis block
    -- already created for current epoch. And we will be in situation
    -- where best chain doesn't have genesis block for current epoch.
    -- If then we need to create block in current epoch, it will be
    -- definitely invalid. To prevent it we create genesis block after
    -- possible revert. Note that createGenesisBlock function will
    -- create block only for epoch which is one more than epoch of
    -- head, so we don't perform such check here.  Also note that it
    -- is not strictly necessary, because we have `canCreateBlock`
    -- which prevents us from creating block when we are not ready,
    -- but it is still good as an optimization. Even if later we see
    -- that there were other valid blocks in old epoch, we will
    -- replace chain and everything will be fine.
    _ <- createGenesisBlock knownEpoch
    return $ PBRgood (toRollback, blocks)

-- | Do all necessary changes when new slot starts.
processNewSlot :: SlotId -> Update (Maybe (GenesisBlock SscDynamicState))
processNewSlot sId = do
    knownSlot <- use slotId
    if sId > knownSlot
       then processNewSlotDo sId
       else pure Nothing

processNewSlotDo :: SlotId -> Update (Maybe (GenesisBlock SscDynamicState))
processNewSlotDo sId@SlotId {..} = do
    slotId .= sId
    mGenBlock <-
      if siSlot == 0
         then createGenesisBlock siEpoch
         else pure Nothing
    blkCleanUp sId
    sscPrepareToNewSlot sId $> mGenBlock

-- We create genesis block for i-th epoch when head of currently known
-- best chain is MainBlock corresponding to one of last `k` slots of
-- (i - 1)-th epoch. Main check is that epoch is (last stored epoch +
-- 1), but we also don't want to create genesis block on top of blocks
-- from previous epoch which are not from last k slots, because it's
-- practically impossible for them to be valid.
shouldCreateGenesisBlock :: EpochIndex -> Query Bool
-- Genesis block for 0-th epoch is hardcoded.
shouldCreateGenesisBlock 0 = pure False
shouldCreateGenesisBlock epoch = doCheckSoft . either (`SlotId` 0) identity <$> getHeadSlot
  where
    -- While we are in process of active development, practically impossible
    -- situations can happen, so we take them into account. We will think about
    -- this check later.
    doCheckSoft SlotId {..} = siEpoch == epoch - 1
    -- TODO add logWarning on `doCheckStrict` failing
    -- doCheckStrict SlotId {..} = siEpoch == epoch - 1 && siSlot >= 5 * k

createGenesisBlock :: EpochIndex -> Update (Maybe (GenesisBlock SscDynamicState))
createGenesisBlock epoch = do
    --readerToState getHeadSlot >>= \hs ->
    --  identity $! traceM $ "[~~~~~~] createGenesisBlock: epoch="
    --                       <> pretty epoch <> ", headSlot=" <> pretty (either (`SlotId` 0) identity hs)
    ifM (readerToState $ shouldCreateGenesisBlock epoch)
        (Just <$> createGenesisBlockDo epoch)
        (pure Nothing)

createGenesisBlockDo :: EpochIndex -> Update (GenesisBlock SscDynamicState)
createGenesisBlockDo epoch = do
    --traceMpcLastVer
    leaders <- readerToState $ calculateLeaders epoch
    genBlock <- blkCreateGenesisBlock epoch leaders
    -- Genesis block contains no transactions,
    --    so we should update only MPC
    sscApplyBlocks $ Left genBlock :| []
    pure genBlock

calculateLeaders :: EpochIndex -> Query SlotLeaders
calculateLeaders epoch = do
    depth <- fromMaybe onErrorGetDepth <$> getMpcCrucialDepth epoch
    utxo <- fromMaybe onErrorGetUtxo <$> getUtxoByDepth depth
    -- TODO: overall 'calculateLeadersDo' gets utxo twice, could be optimised
    threshold <- fromMaybe onErrorGetThreshold <$> getThreshold epoch
    either onErrorCalcLeaders identity <$> Mpc.calculateLeaders utxo threshold
  where
    onErrorGetDepth =
        panic "Depth of MPC crucial slot isn't reasonable"
    onErrorGetUtxo =
        panic "Failed to get utxo necessary for leaders calculation"
    onErrorGetThreshold =
        panic "Failed to get threshold necessary for leaders calculation"
    onErrorCalcLeaders e =
        panic (sformat ("Leaders calculation reported error: " % build) e)

-- | Get keys of nodes participating in an epoch. A node participates if,
-- when there were 'k' slots left before the end of the previous epoch, both
-- of these were true:
--
--   1. It was a stakeholder.
--   2. It had already sent us its VSS key by that time.
getParticipants :: EpochIndex -> Query (Maybe (NonEmpty VssPublicKey))
getParticipants epoch = do
    mDepth <- getMpcCrucialDepth epoch
    mUtxo <- getUtxoByDepth .=<<. mDepth
    mKeymap <-
        fmap _mdVssCertificates <$> (sscGetGlobalPayloadByDepth .=<<. mDepth)
    return $
        do utxo <- mUtxo
           keymap <- mKeymap
           let stakeholders =
                   nub $ map (getAddress . txOutAddress) (toList utxo)
           nonEmpty $
               map signedValue $ mapMaybe (`HM.lookup` keymap) stakeholders

-- slot such that data after it is used for MPC in given epoch
mpcCrucialSlot :: EpochIndex -> SlotId
mpcCrucialSlot 0     = SlotId {siEpoch = 0, siSlot = 0}
mpcCrucialSlot epoch = SlotId {siEpoch = epoch - 1, siSlot = 5 * k - 1}

getMpcCrucialDepth :: EpochIndex -> Query (Maybe Word)
getMpcCrucialDepth epoch = do
    let crucialSlot = mpcCrucialSlot epoch
    (depth, slot) <- getSlotDepth crucialSlot
    if flattenSlotId slot + 2 * k < flattenSlotId (SlotId epoch 0)
        then return Nothing
        else return (Just depth)

getThreshold :: EpochIndex -> Query (Maybe Threshold)
getThreshold epoch = do
    psMaybe <- getParticipants epoch
    return $
        do ps <- psMaybe
           let len = length ps
           return (toInteger (len `div` 2 + len `mod` 2))

processSscMessage :: SscMessage SscDynamicState -> Update Bool
processSscMessage = sscProcessMessage

setToken :: SscToken SscDynamicState -> Update ()
setToken = sscSetToken
