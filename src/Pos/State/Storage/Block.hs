{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedLists        #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE ViewPatterns           #-}

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
       , getSlotDepth
       , mayBlockBeUseful

       , blkCleanUp
       , blkCreateGenesisBlock
       , blkCreateNewBlock
       , blkProcessBlock
       , blkRollback
       , blkSetHead
       ) where

import           Control.Lens            (at, ix, makeClassy, preview, use, uses, view,
                                          (%=), (.=), (.~), (<~), (^.), _Just)
import           Data.Default            (Default, def)
import           Data.List               ((!!))
import           Data.List.NonEmpty      (NonEmpty ((:|)), (<|))
import           Data.SafeCopy           (SafeCopy (..), contain, safeGet, safePut)
import           Data.Vector             (Vector)
import qualified Data.Vector             as V
import           Formatting              (build, sformat, (%))
import           Serokell.Util.Verify    (VerificationRes (..), isVerFailure,
                                          isVerSuccess, verifyGeneric)
import           Universum

import           Pos.Constants           (k)
import           Pos.Crypto              (PublicKey, SecretKey, hash)
import           Pos.Genesis             (genesisLeaders)
import           Pos.Ssc.Class.Types     (SscTypes (..))
import           Pos.State.Storage.Types (AltChain, ProcessBlockRes (..), mkPBRabort)
import           Pos.Types               (Block, BlockHeader, ChainDifficulty, EpochIndex,
                                          GenesisBlock, HeaderHash, MainBlock,
                                          MainBlockHeader, SlotId (..), SlotLeaders, Tx,
                                          VerifyBlockParams (..), VerifyHeaderExtra (..),
                                          blockHeader, blockLeaders, blockSlot,
                                          difficultyL, epochIndexL, gbHeader,
                                          getBlockHeader, headerHash, headerSlot,
                                          mkGenesisBlock, mkMainBlock, mkMainBody,
                                          prevBlockL, siEpoch, verifyBlock, verifyBlocks,
                                          verifyHeader)
import           Pos.Util                (readerToState, _neHead, _neLast)

data BlockStorage ssc = BlockStorage
    { -- | All blocks known to the node. Blocks have pointers to other
      -- blocks and can be easily traversed.
      _blkBlocks        :: !(HashMap (HeaderHash ssc) (Block ssc))
    , -- | Hashes of genesis blocks in the __best chain__.
      _blkGenesisBlocks :: !(Vector (HeaderHash ssc))
    , -- | Hash of the head in the __best chain__.
      _blkHead          :: !(HeaderHash ssc)
    , -- | Alternative chains which can be merged into main chain.
      -- TODO: storing blocks more than once is inefficient, but we
      -- don't care now.
      _blkAltChains     :: ![AltChain ssc]
    , -- | Difficulty of the block with depth `k` (or 0 if there are
      -- less than `k` blocks). It doesn't make sense to consider
      -- blocks with lower difficulty because they certainly fork too
      -- much.
      _blkMinDifficulty :: !ChainDifficulty
    }

makeClassy ''BlockStorage


instance SscTypes ssc => SafeCopy (BlockStorage ssc) where
    getCopy =
        contain $
        do _blkBlocks <- safeGet
           _blkGenesisBlocks <- safeGet
           _blkHead <- safeGet
           _blkAltChains <- safeGet
           _blkMinDifficulty <- safeGet
           return $! BlockStorage {..}
    putCopy BlockStorage {..} =
        contain $
        do safePut _blkBlocks
           safePut _blkGenesisBlocks
           safePut _blkHead
           safePut _blkAltChains
           safePut _blkMinDifficulty

genesisBlock0 :: SscTypes ssc => Block ssc
genesisBlock0 = Left (mkGenesisBlock Nothing 0 genesisLeaders)

genesisBlock0Hash :: SscTypes ssc => HeaderHash ssc
genesisBlock0Hash = hash $ genesisBlock0 ^. blockHeader

instance SscTypes ssc => Default (BlockStorage ssc) where
    def =
        BlockStorage
        { _blkBlocks = [(genesisBlock0Hash, genesisBlock0)]
        , _blkGenesisBlocks = [genesisBlock0Hash]
        , _blkHead = genesisBlock0Hash
        , _blkAltChains = mempty
        , _blkMinDifficulty = (genesisBlock0 @ssc) ^. difficultyL
        }

type Query ssc a = forall m x. (HasBlockStorage x ssc, MonadReader x m) => m a
type Update ssc a = forall m x. (HasBlockStorage x ssc, MonadState x m) => m a

-- | Get block by hash of its header.
getBlock :: HeaderHash ssc -> Query ssc (Maybe (Block ssc))
getBlock h = view (blkBlocks . at h)

-- | Get block by its depth, i. e. number of times one needs to use
-- pointer to previous block.
-- TODO: optimize using blkGenesisBlocks.
getBlockByDepth :: Word -> Query ssc (Maybe (Block ssc))
getBlockByDepth i = do
    headHash <- view blkHead
    getBlockByDepthDo i headHash

getBlockByDepthDo :: Word -> HeaderHash ssc -> Query ssc (Maybe (Block ssc))
getBlockByDepthDo 0 h = getBlock h
getBlockByDepthDo i h =
    maybe (pure Nothing) (getBlockByDepthDo (i - 1) . view prevBlockL) =<<
    getBlock h

-- | Get block which is the head of the __best chain__.
getHeadBlock :: Query ssc (Block ssc)
getHeadBlock = do
    headHash <- view blkHead
    let errorMsg =
            sformat ("blkHead (" %build % " is not found in storage") headHash
    fromMaybe (panic errorMsg) <$> getBlockByDepth 0

-- | Get list of slot leaders for the given epoch if it is known.
getLeaders :: EpochIndex -> Query ssc (Maybe SlotLeaders)
getLeaders (fromIntegral -> epoch) = do
    blkIdx <- preview (blkGenesisBlocks . ix epoch)
    maybe (pure Nothing) (fmap leadersFromBlock . getBlock) blkIdx
  where
    leadersFromBlock (Just (Left genBlock)) = Just $ genBlock ^. blockLeaders
    leadersFromBlock _                      = Nothing

-- | Get leader of the given slot if it's known.
getLeader :: SlotId -> Query ssc (Maybe PublicKey)
getLeader SlotId {..} = (preview $ _Just . ix (fromIntegral siSlot)) <$> getLeaders siEpoch

-- | Get depth of the first main block whose SlotId ≤ given value.
-- Depth of the deepest (i. e. 0-th genesis) block is returned if
-- there is no such block.
-- SlotId of such block is also returned (for genesis block siSlot is set to 0).
getSlotDepth :: SlotId -> Query ssc (Word, SlotId)
getSlotDepth slotId = do
    headBlock <- getHeadBlock
    getSlotDepthDo 0 headBlock
  where
    getSlotDepthDo :: Word -> Block ssc -> Query ssc (Word, SlotId)
    getSlotDepthDo depth (Right blk)
        | blk ^. blockSlot <= slotId = pure (depth, blk ^. blockSlot)
    getSlotDepthDo depth blk =
        maybe (pure (depth, blkSlot blk)) (getSlotDepthDo (depth + 1)) =<<
        getBlock (blk ^. prevBlockL)
    blkSlot (Left genBlk)   = SlotId (genBlk ^. epochIndexL) 0
    blkSlot (Right mainBlk) = mainBlk ^. blockSlot

-- | Check that block header is correct and claims to represent block
-- which may become part of blockchain.
mayBlockBeUseful
    :: SscTypes ssc
    => SlotId -> MainBlockHeader ssc -> Query ssc VerificationRes
mayBlockBeUseful currentSlotId header = do
    let hSlot = header ^. headerSlot
    leaders <- getLeaders (siEpoch hSlot)
    isInteresting <- isHeaderInteresting header
    let vhe = def {vheCurrentSlot = Just currentSlotId, vheLeaders = leaders}
    let extraChecks =
            [ ( isInteresting
              , "block is not more difficult than the best known block and \
                 \can't be appended to alternative chain")
            ]
    return $ verifyHeader vhe (Right header) <> verifyGeneric extraChecks

isHeaderInteresting :: SscTypes ssc => MainBlockHeader ssc -> Query ssc Bool
isHeaderInteresting header = do
    altChains <- view blkAltChains
    or <$> mapM ($ header) (isMostDifficult : map canContinueAltChain altChains)

canContinueAltChain
    :: SscTypes ssc
    => AltChain ssc -> MainBlockHeader ssc -> Query ssc Bool
canContinueAltChain (blk :| _) header
    | isVerFailure $ verifyHeader vhe (Right header) = pure False
    | otherwise = (header ^. difficultyL >=) <$> view blkMinDifficulty
  where
    vhe = def {vheNextHeader = Just (blk ^. blockHeader)}

isMostDifficult :: MainBlockHeader ssc -> Query ssc Bool
isMostDifficult (view difficultyL -> difficulty) =
    (difficulty >) . view difficultyL <$> getHeadBlock

insertBlock :: SscTypes ssc => Block ssc -> Update ssc ()
insertBlock blk = blkBlocks . at (headerHash blk) .= Just blk

-- | Process received block, adding it to alternative chain if
-- necessary. This block won't become part of main chain, the only way
-- to do it is to use `blkSetHead`. This function only caches block if
-- necessary.
blkProcessBlock
    :: SscTypes ssc
    => SlotId -> Block ssc -> Update ssc (ProcessBlockRes ssc)
blkProcessBlock currentSlotId blk = do
    -- First of all we do the simplest general checks.
    leaders <-
        either
            (const $ pure Nothing)
            (readerToState . getLeaders . siEpoch . view blockSlot)
            blk
    let vhe = def {vheCurrentSlot = Just currentSlotId, vheLeaders = leaders}
    let header = blk ^. blockHeader
    let verRes =
            mconcat
                [ verifyHeader vhe header
                , verifyBlock (def {vbpVerifyGeneric = True}) blk
                ]
    case verRes of
        VerFailure errors -> pure $ mkPBRabort errors
        VerSuccess        -> blkProcessBlockDo blk

blkProcessBlockDo
    :: SscTypes ssc
    => Block ssc -> Update ssc (ProcessBlockRes ssc)
blkProcessBlockDo blk = do
    -- At this point we know that block is good in isolation.
    -- Our first attempt is to continue the best chain and finish.
    ifM (readerToState $ canContinueBestChain blk)
        -- If it's possible, we just do it.
        (continueBestChain blk)
        -- Our next attempt is to start alternative chain.
        (maybe (tryContinueAltChain blk) pure =<< tryStartAltChain blk)

canContinueBestChain :: SscTypes ssc => Block ssc -> Query ssc Bool
-- We don't continue best chain with received genesis block. It is
-- added automatically when last block in epoch is added.
canContinueBestChain (Left _) = pure False
canContinueBestChain blk = do
    headBlk <- getHeadBlock
    -- At this point we only need to check that block references head
    -- and is consistent with it.
    let vhe = def {vhePrevHeader = Just $ getBlockHeader headBlk}
    let vbp = def {vbpVerifyHeader = Just vhe}
    return $ isVerSuccess $ verifyBlock vbp blk

-- We know that we can continue best chain, but we also try to merge
-- alternative chain. If we succeed, we do it, instead of adopting a
-- single blockl.
continueBestChain
    :: SscTypes ssc
    => Block ssc -> Update ssc (ProcessBlockRes ssc)
continueBestChain blk = do
    insertBlock blk
    decideWhatToDo <$> tryContinueAltChain blk
  where
    decideWhatToDo r@(PBRgood _) = r
    decideWhatToDo _             = PBRgood (0, blk :| [])

-- Possible results are:
-- • Nothing: can't start alternative chain.
-- • Just PBRgood: started alternative chain and can merge it already.
-- • Just PBRmore: started alternative chain and want more.
tryStartAltChain
    :: SscTypes ssc
    => Block ssc -> Update ssc (Maybe (ProcessBlockRes ssc))
tryStartAltChain (Left _) = pure Nothing
tryStartAltChain (Right blk) =
    -- TODO: more checks should be done here probably
    ifM (readerToState $ isMostDifficult (blk ^. gbHeader))
        (Just <$> startAltChain blk)
        (pure Nothing)

-- Here we know that block may represent a valid chain which
-- potentially can become main chain. We put it into map with all
-- blocks and add new AltChain.
-- PBRgood is returned if chain can already be merged.
-- PBRmore is returned if more blocks are needed.
startAltChain
    :: SscTypes ssc
    => MainBlock ssc -> Update ssc (ProcessBlockRes ssc)
startAltChain blk = do
    insertBlock $ Right blk
    blkAltChains %= ((Right blk :| []) :)
    -- 0 is passed here, because we put new chain into head of blkAltChains
    maybe (PBRmore $ blk ^. prevBlockL) PBRgood <$> tryMergeAltChain 0

pbrUseless :: ProcessBlockRes ssc
pbrUseless = mkPBRabort ["block can't be added to any chain"]

-- Here we try to continue one of known alternative chains. It may
-- happen that common ancestor with main chain will be found. In this
-- case we return PBRgood and expect `blkRollback` and `blkSetHeader`
-- to be called.
tryContinueAltChain
    :: forall ssc. SscTypes ssc
    => Block ssc -> Update ssc (ProcessBlockRes ssc)
tryContinueAltChain blk = do
    n <- length <$> use blkAltChains
    foldM go pbrUseless ([0 .. n - 1] :: Vector Int)
  where
    go :: ProcessBlockRes ssc -> Int -> Update ssc (ProcessBlockRes ssc)
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
tryContinueAltChainDo
    :: SscTypes ssc
    => Block ssc -> Int -> Update ssc (ProcessBlockRes ssc)
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
continueAltChain
    :: SscTypes ssc
    => Block ssc -> Int -> Update ssc (ProcessBlockRes ssc)
continueAltChain blk i = do
    insertBlock blk
    blkAltChains . ix i %= (blk <|)
    maybe (PBRmore $ blk ^. prevBlockL) PBRgood <$> tryMergeAltChain i

-- Try to merge alternative chain into the main chain.
-- On success number of blocks to rollback is returned, as well as chain which can be merged.
-- Note that it doesn't actually merge chain, more checks are required before merge.
tryMergeAltChain
    :: SscTypes ssc
    => Int -> Update ssc (Maybe (Word, AltChain ssc))
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
tryMergeAltChainDo :: SscTypes ssc => AltChain ssc -> Query ssc (Maybe Word)
tryMergeAltChainDo altChain = do
    let altChainDifficulty = altChain ^. _neLast . difficultyL
    isHardest <- (altChainDifficulty >) . view difficultyL <$> getHeadBlock
    if not isHardest
        then return Nothing
        else do
            rollback <- findRollback k (altChain ^. _neHead . prevBlockL)
            case rollback of
                Nothing -> return Nothing
                Just x ->
                    ifM
                        (testMergeAltChain x altChain)
                        (return rollback)
                        (return Nothing)

findRollback
    :: forall ssc. SscTypes ssc
    => Word -> HeaderHash ssc -> Query ssc (Maybe Word)
findRollback maxDepth neededParent =
    findRollbackDo 0 . getBlockHeader =<< getHeadBlock
  where
    findRollbackDo :: Word -> BlockHeader ssc -> Query ssc (Maybe Word)
    findRollbackDo res header
        | res > maxDepth = pure Nothing
        | headerHash header == neededParent = pure . pure $ res
        | otherwise =
            maybe (pure Nothing) (findRollbackDo (res + 1) . getBlockHeader) =<<
            getBlock (header ^. prevBlockL)

-- Before reporting that AltChain can be merged, we verify whole
-- result to be sure that nothing went wrong.
-- We ignore check related to current slot, because we ensure that no blocks
-- from non-existing slot can appear in this storage.
testMergeAltChain :: SscTypes ssc => Word -> AltChain ssc -> Query ssc Bool
testMergeAltChain toRollback altChain =
    isVerSuccess . verifyBlocks Nothing . (++ toList altChain) <$>
    blocksToTestMerge toRollback

-- This function collects all blocks from main chain which must be
-- included into sequence of blocks further passed to verifyBlocks.
-- We need to include genesis block for epoch where merge happens, so that
-- verification function can check leaders.
blocksToTestMerge :: Word -> Query ssc [Block ssc]
blocksToTestMerge toRollback =
    fmap reverse . blocksToTestMergeDo . fromMaybe doPanic =<<
    getBlockByDepth toRollback
  where
    doPanic = panic "blocksToTestMerge: attempt to rollback too much"

-- We know common ancestor, now we need to reach genesis block.
blocksToTestMergeDo :: Block ssc -> Query ssc [Block ssc]
blocksToTestMergeDo commonAncestor =
    case commonAncestor of
        Left _ -> pure [commonAncestor]
        Right _ -> do
            prevBlock <- getBlock $ commonAncestor ^. prevBlockL
            case prevBlock of
                Nothing  -> panic "impossible happened in blocksToTestMergeDo"
                Just blk -> (commonAncestor :) <$> blocksToTestMergeDo blk

-- | Create a new block and append it to the best chain.
blkCreateNewBlock
    :: SscTypes ssc
    => SecretKey
    -> SlotId
    -> [Tx]
    -> SscPayload ssc
    -> Update ssc (MainBlock ssc)
blkCreateNewBlock sk sId txs mpcData = do
    prevHeader <- readerToState $ getBlockHeader <$> getHeadBlock
    let body = mkMainBody txs mpcData
    let blk = mkMainBlock (Just prevHeader) sId sk body
    insertBlock $ Right blk
    blk <$ blkSetHead (headerHash blk)

blkCreateGenesisBlock :: SscTypes ssc => EpochIndex -> SlotLeaders -> Update ssc (GenesisBlock ssc)
blkCreateGenesisBlock epoch leaders = do
    prevHeader <- readerToState $ getBlockHeader <$> getHeadBlock
    let blk = mkGenesisBlock (Just prevHeader) epoch leaders
    insertBlock $ Left blk
    let h = headerHash blk
    -- when we create genesis block, two situations are possible:
    -- • it's created for new epoch, then we append it to blkGenesisBlocks
    -- • it's created for epoch started earlier because of rollback,
    -- which means that we replace last element in blkGenesisBlocks.
    -- Other situations are illegal.
    -- Note that we don't need to remove last element from blkGenesisBlocks
    -- when rollback happens, because we ensure that if genesis block was
    -- created for some epoch, then we'll always have genesis block for
    -- this epoch.
    blkGenesisBlocks %= appendOrSet (fromIntegral epoch) h
    blk <$ blkSetHead h

appendOrSet :: Int -> a -> Vector a -> Vector a
appendOrSet idx val vec
    | length vec == idx = vec `V.snoc` val
    | length vec - 1 == idx = vec & ix idx .~ val
    | otherwise =
        panic "appendOrSet: idx is not last and is not right after last"

-- | Set head of main blockchain to block which is guaranteed to
-- represent valid chain and be stored in blkBlocks.
blkSetHead :: HeaderHash ssc -> Update ssc ()
blkSetHead headHash = do
    blkHead .= headHash
    blkMinDifficulty <~ maybe 0 (view difficultyL) <$>
        readerToState (getBlockByDepth k)

-- | Rollback last `n` blocks.
blkRollback :: SscTypes ssc => Word -> Update ssc ()
blkRollback =
    blkSetHead . maybe genesisBlock0Hash (hash . getBlockHeader) <=<
    readerToState . getBlockByDepth

-- | Remove obsolete cached blocks, alternative chains which are
-- definitely useless, etc.
-- TODO
blkCleanUp :: SlotId -> Update ssc ()
blkCleanUp _ = do
    headDifficulty <- view difficultyL <$> readerToState getHeadBlock
    blkAltChains %= (filter $ (> headDifficulty) . view (_neLast . difficultyL))
    blkBlocks %= identity  -- TODO
