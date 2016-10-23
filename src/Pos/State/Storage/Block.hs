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

       , blkCleanUp
       , blkProcessBlock
       , blkRollback
       , blkSetHead
       ) where

import           Control.Lens            (at, ix, makeClassy, preview, use, uses, view,
                                          views, (%=), (.=), (<~), (^.), (^?))
import           Data.Default            (Default, def)
import qualified Data.HashMap.Strict     as HM
import           Data.List               ((!!))
import           Data.List.NonEmpty      (NonEmpty ((:|)), (<|))
import           Data.SafeCopy           (base, deriveSafeCopySimple)
import           Data.Vector             (Vector)
import           Serokell.Util.Verify    (VerificationRes (..), isVerFailure,
                                          isVerSuccess, verifyGeneric)
import           Universum

import           Pos.Constants           (k)
import           Pos.Crypto              (PublicKey, hash)
import           Pos.Genesis             (genesisLeaders)
import           Pos.State.Storage.Types (AltChain, ProcessBlockRes (..), mkPBRabort)
import           Pos.Types               (Block, ChainDifficulty, EpochIndex, HeaderHash,
                                          MainBlock, MainBlockHeader, SlotId (..),
                                          SlotLeaders, VerifyBlockParams (..),
                                          VerifyHeaderExtra (..), blockHeader,
                                          blockLeaders, blockSlot, difficultyL, gbHeader,
                                          getBlockHeader, headerHash, headerSlot,
                                          mkGenesisBlock, prevBlockL, siEpoch,
                                          verifyBlock, verifyHeader)
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
      -- TODO: storing blocks more than once is inefficient, but we
      -- don't care now.
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

getLeadersMaybe :: EpochIndex -> Query (Maybe SlotLeaders)
getLeadersMaybe = fmap f . getLeaders
  where
    f v
        | null v = Nothing
        | otherwise = Just v

-- | Get leader of the given slot if it's known.
getLeader :: SlotId -> Query (Maybe PublicKey)
getLeader SlotId {..} = (^? ix (fromIntegral siSlot)) <$> getLeaders siEpoch

-- | Check that block header is correct and claims to represent block
-- which may become part of blockchain.
mayBlockBeUseful :: SlotId -> MainBlockHeader -> Query VerificationRes
mayBlockBeUseful currentSlotId header = do
    let hSlot = header ^. headerSlot
    leaders <- getLeadersMaybe (siEpoch hSlot)
    isInteresting <- isHeaderInteresting header
    isKnown <- views blkBlocks (HM.member (hash $ Right header))
    let vhe = def {vheCurrentSlot = Just currentSlotId, vheLeaders = leaders}
    let extraChecks =
            [ (not isKnown, "block is already known")
            , ( isInteresting
              , "block is not more difficult than the best known block and \
                 \can't be appended to alternative chain")
            ]
    return $ verifyHeader vhe (Right header) <> verifyGeneric extraChecks

isHeaderInteresting :: MainBlockHeader -> Query Bool
isHeaderInteresting header = do
    altChains <- view blkAltChains
    or <$> mapM ($ header) (isMostDifficult : map canContinueAltChain altChains)

canContinueAltChain :: AltChain -> MainBlockHeader -> Query Bool
canContinueAltChain (blk :| _) header
    | isVerFailure $ verifyHeader vhe (Right header) = pure False
    | otherwise = (header ^. difficultyL >=) <$> view blkMinDifficulty
  where
    vhe = def {vheNextHeader = Just (blk ^. blockHeader)}

isMostDifficult :: MainBlockHeader -> Query Bool
isMostDifficult (view difficultyL -> difficulty) =
    (difficulty >) . view difficultyL <$> getHeadBlock

insertBlock :: Block -> Update ()
insertBlock blk = blkBlocks . at (headerHash blk) .= Just blk

-- | Process received block, adding it to alternative chain if
-- necessary. This block won't become part of main chain, the only way
-- to do it is to use `blkSetHead`. This function only caches block if
-- necessary.
blkProcessBlock :: SlotId -> Block -> Update ProcessBlockRes
blkProcessBlock currentSlotId blk = do
    -- First of all we do the simplest general checks.
    leaders <-
        either
            (const $ pure Nothing)
            (readerToState . getLeadersMaybe . siEpoch . view blockSlot)
            blk
    let vhe = def {vheCurrentSlot = Just currentSlotId, vheLeaders = leaders}
    let header = blk ^. blockHeader
    isKnown <- readerToState $ views blkBlocks (HM.member (hash header))
    let verRes =
            mconcat
                [ verifyGeneric [(not isKnown, "block is already known")]
                , verifyHeader vhe header
                , verifyBlock (def {vbpVerifyGeneric = True}) blk
                ]
    case verRes of
        VerFailure errors -> pure $ mkPBRabort errors
        VerSuccess        -> blkProcessBlockDo blk

blkProcessBlockDo :: Block -> Update ProcessBlockRes
blkProcessBlockDo blk = do
    -- At this point we know that block is good in isolation.
    -- Our first attempt is to continue the best chain and finish.
    continueMain <- readerToState $ canContinueBestChain blk
    if continueMain
        then PBRgood (0, blk :| []) <$ insertBlock blk
        -- Our next attempt is to start alternative chain.
        else ifM (tryStartAltChain blk)
                 (return $ PBRmore $ headerHash blk)
                 (tryContinueAltChain blk)

canContinueBestChain :: Block -> Query Bool
-- We don't continue best chain with received genesis block. It is
-- added automatically when last block in epoch is added.
-- TODO: it's not done now actually.
canContinueBestChain (Left _) = pure False
canContinueBestChain blk = do
    headBlk <- getHeadBlock
    -- At this point we only need to check that block references head
    -- and is consistent with it.
    let vhe = def {vhePrevHeader = Just $ getBlockHeader headBlk}
    let vbp = def {vbpVerifyHeader = Just vhe}
    return $ isVerSuccess $ verifyBlock vbp blk

tryStartAltChain :: Block -> Update Bool
tryStartAltChain (Left _) = pure False
tryStartAltChain (Right blk) = do
    isMostDiff <- readerToState $ isMostDifficult (blk ^. gbHeader)
    -- TODO: more checks should be done here probably
    if isMostDiff
        then True <$ startAltChain blk
        else pure False

-- Here we know that block may represent a valid chain which
-- potentially can become main chain. We put it into map with all
-- blocks and add new AltChain.
startAltChain :: MainBlock -> Update ()
startAltChain blk = do
    insertBlock $ Right blk
    blkAltChains %= ((Right blk :| []) :)

pbrUseless :: ProcessBlockRes
pbrUseless = (mkPBRabort ["block can't be added to any chain"])

-- Here we try to continue one of known alternative chains. It may
-- happen that common ancestor with main chain will be found. In this
-- case we return PBRgood and expect `blkRollback` and `blkSetHeader`
-- to be called.
tryContinueAltChain :: Block -> Update ProcessBlockRes
tryContinueAltChain blk = do
    n <- length <$> use blkAltChains
    foldM go pbrUseless ([0 .. n - 1] :: Vector Int)
  where
    go :: ProcessBlockRes -> Int -> Update ProcessBlockRes
    -- PBRgood means that chain can be merged into main chain.
    -- In this case we stop processing.
    go good@(PBRgood _) _ = pure good
    -- PBRmore means that block has been added to at least one
    -- alternative chain, we return PBRmore, but try to add it to
    -- other chains as well.
    go more@(PBRmore _) i =  more <$ tryContinueAltChainDo blk i
    -- PBRabort means that we didn't add block to alternative
    -- chains. In this case we just go further and try another chain.
    go (PBRabort _) i     = tryContinueAltChainDo blk i

-- Here we actually try to continue concrete AltChain (given its index).
tryContinueAltChainDo :: Block -> Int -> Update ProcessBlockRes
tryContinueAltChainDo blk i = do
    -- We only need to check that block can be previous block of the
    -- head of alternative chain.
    (altChainBlk :| _) <- uses blkAltChains (!! i)
    let vhe = def {vheNextHeader = Just $ altChainBlk ^. blockHeader}
    let vbp = def {vbpVerifyHeader = Just vhe}
    if isVerSuccess $ verifyBlock vbp blk
        then continueAltChain blk i
        else pure pbrUseless

-- Here we know that block is a good continuation of i-th chain.
continueAltChain :: Block -> Int -> Update ProcessBlockRes
continueAltChain blk i = do
    blkAltChains . ix i %= (blk <|)
    maybe (PBRmore $ headerHash blk) PBRgood <$> tryMergeAltChain i

-- Try to merge alternative chain into the main chain.
-- On success number of blocks to rollback is returned, as well as chain which can be merged.
-- Note that it doesn't actually merge chain, more checks are required before merge.
tryMergeAltChain :: Int -> Update (Maybe (Word, AltChain))
tryMergeAltChain i = do
    altChain <- uses blkAltChains (!! i)
    toRollback <- readerToState $ tryMergeAltChainDo altChain
    case toRollback of
        Nothing -> return Nothing
        -- Note that it's safe to remove i-th element here, because we stop
        -- at the first `PBRgood`. This is fragile though.
        Just x  -> Just (x, altChain) <$ (blkAltChains %= removeIth i)

removeIth :: Int -> [x] -> [x]
removeIth i xs =
    let (l, (_:r)) = splitAt i xs
    in l ++ r

-- Here we actually try to merge alternative chain into main
-- chain. Note that it's only a query, so actual merge won't be
-- performed.
tryMergeAltChainDo :: AltChain -> Query (Maybe Word)
tryMergeAltChainDo = notImplemented

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

-- | Remove obsolete cached blocks, alternative chains which are
-- definitely useless, etc.
-- TODO
blkCleanUp :: SlotId -> Update ()
blkCleanUp _ = blkBlocks %= identity
