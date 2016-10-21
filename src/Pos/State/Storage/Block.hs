{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Blocks maintenance happens here.

module Pos.State.Storage.Block
       (
         BlockStorage
       , HasBlockStorage (blockStorage)

       , getBlock
       , getBlockByDepth
       , getHeadBlock
       , getLeader
       , getLeaders
       , mayBlockBeUseful

       , blkProcessBlock
       , blkRollback
       , blkSetHead
       ) where

import           Control.Lens            (at, ix, makeClassy, preview, view, views, (.=),
                                          (<~), (^.), (^?))
import           Data.Default            (Default, def)
import qualified Data.HashMap.Strict     as HM
import           Data.List.NonEmpty      (NonEmpty ((:|)))
import           Data.SafeCopy           (base, deriveSafeCopySimple)
import           Data.Vector             (Vector)
import           Serokell.Util.Verify    (VerificationRes, verifyGeneric)
import           Universum

import           Pos.Constants           (epochSlots, k)
import           Pos.Crypto              (PublicKey, hash)
import           Pos.Genesis             (genesisLeaders)
import           Pos.State.Storage.Types (AltChain, ProcessBlockRes (..))
import           Pos.Types               (Block, ChainDifficulty, EpochIndex, HeaderHash,
                                          MainBlockHeader, SlotId (..), SlotLeaders,
                                          blockHeader, blockLeaders, difficultyL,
                                          getBlockHeader, headerLeaderKey, headerSlot,
                                          mkGenesisBlock, prevBlockL, siEpoch,
                                          verifyHeader)
import           Pos.Util                (readerToState)

data BlockStorage = BlockStorage
    { -- | All blocks known to the node. Blocks have pointers to other
      -- blocks and can be easily traversed.
      _blkBlocks        :: !(HashMap HeaderHash Block)
    , -- | Hashes of genesis blocks in the __best chain__.
      _blkGenesisBlocks :: !(Vector HeaderHash)
    , -- | Hash of the head in the __best chain__.
      _blkHead          :: !HeaderHash
    , -- | Alternative chains which can be merged into main chain.
      _blkAltChains     :: ![AltChain]
    , -- | Difficulty of the block with depth `k` (or 0 if there are
      -- less than `k` blocks). It doesn't make sense to consider
      -- blocks with lower difficulty because they certainly fork too
      -- much.
      _blkMinDifficulty :: !ChainDifficulty
    }

makeClassy ''BlockStorage
deriveSafeCopySimple 0 'base ''BlockStorage

genesisBlock0 :: Block
genesisBlock0 = Left (mkGenesisBlock Nothing 0 genesisLeaders)

genesisBlock0Hash :: HeaderHash
genesisBlock0Hash = hash $ genesisBlock0 ^. blockHeader

instance Default BlockStorage where
    def =
        BlockStorage
        { _blkBlocks = [(genesisBlock0Hash, genesisBlock0)]
        , _blkGenesisBlocks = [genesisBlock0Hash]
        , _blkHead = genesisBlock0Hash
        , _blkAltChains = mempty
        , _blkMinDifficulty = genesisBlock0 ^. difficultyL
        }
      where

type Query a = forall m x. (HasBlockStorage x, MonadReader x m) => m a
type Update a = forall m x. (HasBlockStorage x, MonadState x m) => m a

-- | Get block by hash of its header.
getBlock :: HeaderHash -> Query (Maybe Block)
getBlock h = view (blkBlocks . at h)

-- | Get block by its depth, i. e. number of times one needs to use
-- pointer to previous block.
getBlockByDepth :: Word -> Query (Maybe Block)
getBlockByDepth i = do
    headHash <- view blkHead
    getBlockByHeadDo i headHash

getBlockByHeadDo :: Word -> HeaderHash -> Query (Maybe Block)
getBlockByHeadDo 0 h = getBlock h
getBlockByHeadDo i h =
    maybe (pure Nothing) (getBlockByHeadDo (i - 1) . view prevBlockL) =<<
    getBlock h

-- | Get block which is the head of the __best chain__.
getHeadBlock :: Query Block
getHeadBlock = fromMaybe reportError <$> getBlockByDepth 0
  where
    reportError = panic "blkHead is not found in storage"

-- | Get list of slot leaders for the given epoch. Empty list is returned
-- if no information is available.
getLeaders :: EpochIndex -> Query SlotLeaders
getLeaders (fromIntegral -> epoch) = do
    blkIdx <- preview (blkGenesisBlocks . ix epoch)
    maybe (pure mempty) (fmap leadersFromBlock . getBlock) blkIdx
  where
    leadersFromBlock (Just (Left genBlock)) = genBlock ^. blockLeaders
    leadersFromBlock _                      = mempty

-- | Get leader of the given slot if it's known.
getLeader :: SlotId -> Query (Maybe PublicKey)
getLeader SlotId {..} = (^? ix (fromIntegral siSlot)) <$> getLeaders siEpoch

-- | Check that block header is correct and claims to represent block
-- which may become part of blockchain.
mayBlockBeUseful :: SlotId -> MainBlockHeader -> Query VerificationRes
mayBlockBeUseful currentSlotId header = do
    let hSlot = header ^. headerSlot
    expectedLeader <- getLeader hSlot
    isInteresting <- isHeaderInteresting header
    isKnown <- views blkBlocks (HM.member (hash $ Right header))
    let checks =
            [ ( hSlot < currentSlotId
              , "block is from slot which hasn't happened yet")
            , (not isKnown, "block is already known")
            , ( siSlot hSlot < epochSlots
              , "slot index is not less than epochSlots")
            , ( maybe True (== (header ^. headerLeaderKey)) expectedLeader
              , "block's leader is different from expected one")
            , ( isInteresting
              , "block is not more difficult than the best known block and \
                 \can't be appended to alternative chain")
            ]
    return $ verifyHeader (Right header) <> verifyGeneric checks

isHeaderInteresting :: MainBlockHeader -> Query Bool
isHeaderInteresting header = do
    altChains <- view blkAltChains
    or <$> mapM ($ header) (isMostDifficult : map canContinueAltChain altChains)

canContinueAltChain :: AltChain -> MainBlockHeader -> Query Bool
canContinueAltChain (blk :| _) header
    | blk ^. prevBlockL /= hash (Right header) = pure False
    | otherwise = (header ^. difficultyL >=) <$> view blkMinDifficulty

isMostDifficult :: MainBlockHeader -> Query Bool
isMostDifficult (view difficultyL -> difficulty) =
    (difficulty >) . view difficultyL <$> getHeadBlock

-- | Process received block, adding it to alternative chain if
-- necessary. This block won't become part of main chain, the only way
-- to do it is to use `blkSetHead`.
blkProcessBlock :: Block -> Update ProcessBlockRes
blkProcessBlock _ = pure $ PBRabort mempty

-- | Set head of main blockchain to block which is guaranteed to
-- represent valid chain and be stored in blkBlocks.
blkSetHead :: HeaderHash -> Update ()
blkSetHead headHash = do
    blkHead .= headHash
    blkMinDifficulty <~ maybe 0 (view difficultyL) <$>
        readerToState (getBlockByDepth k)

-- | Rollback last `n` blocks.
blkRollback :: Word -> Update ()
blkRollback =
    blkSetHead . maybe genesisBlock0Hash (hash . getBlockHeader) <=<
    readerToState . getBlockByDepth
