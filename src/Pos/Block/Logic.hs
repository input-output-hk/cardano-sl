{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | Logic of blocks processing.

module Pos.Block.Logic
       (
         -- * Headers
         ClassifyHeaderRes (..)
       , classifyNewHeader
       , ClassifyHeadersRes (..)
       , classifyHeaders
       , loadHeadersUntil
       , retrieveHeadersFromTo
       , getHeadersOlderExp
       , lcaWithMainChain

         -- * Blocks
       , applyBlocks
       , rollbackBlocks
       , verifyBlocks
       , withBlkSemaphore
       ) where

import           Control.Lens         (view, (^.))
import           Control.Monad.Catch  (onException)
import           Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import           Data.Default         (Default (def))
import           Data.List.NonEmpty   (NonEmpty ((:|)))
import qualified Data.List.NonEmpty   as NE
import qualified Data.Text            as T
import           Formatting           (int, sformat, (%))
import           Serokell.Util.Verify (VerificationRes (..), formatAllErrors,
                                       isVerSuccess, verResToMonadError)
import           Universum

import           Pos.Constants        (k)
import           Pos.Context          (putBlkSemaphore, takeBlkSemaphore)
import           Pos.Crypto           (hash)
import           Pos.DB               (MonadDB)
import qualified Pos.DB               as DB
import           Pos.Modern.Txp.Logic (txApplyBlocks, txRollbackBlocks, txVerifyBlocks)
import           Pos.Slotting         (getCurrentSlot)
import           Pos.Ssc.Class        (Ssc)
import           Pos.Ssc.Extra        (sscApplyBlocks, sscRollback, sscVerifyBlocks)
import           Pos.Types            (Block, BlockHeader, HeaderHash, Undo,
                                       VerifyHeaderParams (..), blockHeader, difficultyL,
                                       flattenEpochOrSlot, getEpochOrSlot, headerSlot,
                                       prevBlockL, verifyHeader, verifyHeaders,
                                       vhpVerifyConsensus)
import qualified Pos.Types            as Types
import           Pos.WorkMode         (WorkMode)


-- | Result of single (new) header classification.
data ClassifyHeaderRes
    = CHContinues      -- ^ Header continues our main chain.
    | CHAlternative    -- ^ Header continues alternative chain which
                       -- is more difficult.
    | CHUseless !Text  -- ^ Header is useless.
    | CHInvalid !Text  -- ^ Header is invalid.

-- | Make `ClassifyHeaderRes` from list of error messages using
-- `CHRinvalid` constructor. Intended to be used with `VerificationRes`.
-- Note: this version forces computation of all error messages. It can be
-- made more efficient but less informative by using head, for example.
mkCHRinvalid :: [Text] -> ClassifyHeaderRes
mkCHRinvalid = CHInvalid . T.intercalate "; "

-- | Classify new header announced by some node. Result is represented
-- as ClassifyHeaderRes type.
classifyNewHeader
    :: (WorkMode ssc m)
    => BlockHeader ssc -> m ClassifyHeaderRes
-- Genesis headers seem useless, we can create them by ourselves.
classifyNewHeader (Left _) = pure $ CHUseless "genesis header is useless"
classifyNewHeader (Right header) = do
    curSlot <- getCurrentSlot
    -- First of all we check whether header is from current slot and
    -- ignore it if it's not.
    if curSlot == header ^. headerSlot
        then classifyNewHeaderDo <$> DB.getTip <*> DB.getTipBlock
        else return $ CHUseless "header is not for current slot"
  where
    classifyNewHeaderDo tip tipBlock
        -- If header's parent is our tip, we verify it against tip's header.
        | tip == header ^. prevBlockL =
            let vhp =
                    def
                    { vhpVerifyConsensus = True
                    , vhpPrevHeader = Just $ tipBlock ^. blockHeader
                    }
                verRes = verifyHeader vhp (Right header)
            in case verRes of
                   VerSuccess        -> CHContinues
                   VerFailure errors -> mkCHRinvalid errors
        -- If header's parent is not our tip, we check whether it's
        -- more difficult than our main chain.
        | tipBlock ^. difficultyL < header ^. difficultyL = CHAlternative
        -- If header can't continue main chain and is not more
        -- difficult than main chain, it's useless.
        | otherwise =
            CHUseless $
            "header doesn't continue main chain and is not more difficult"

-- | Find lca headers and main chain, including oldest header's parent
-- hash. Headers passed are newest first.
lcaWithMainChain
    :: (WorkMode ssc m)
    => NonEmpty (BlockHeader ssc) -> m (Maybe (HeaderHash ssc))
lcaWithMainChain headers@(h:|hs) =
    fmap fst . find snd <$>
        mapM (\hh -> (hh,) <$> DB.isBlockInMainChain hh)
             (map hash (h : hs) ++ [NE.last headers ^. prevBlockL])
             -- take hash of parent of last BlockHeader and convert all headers to hashes
             -- and reverse

-- | Result of multiple headers classification.
data ClassifyHeadersRes ssc
    = CHsValid (BlockHeader ssc) -- ^ Header list can be applied, LCA attached.
    | CHsUseless !Text          -- ^ Header is useless.
    | CHsInvalid !Text          -- ^ Header is invalid.

-- | Classify headers received in response to 'GetHeaders'
-- message. Should be passed in newest-head order.
--
-- * If there are any errors in chain of headers, CHRinvalid is returned.
-- * If chain of headers is a valid continuation of our main chain,
-- CHRcontinues is returned.
-- * If chain of headers forks from our main chain but not too much,
-- CHRalternative is returned.
-- * If chain of headers forks from our main chain too much, CHRuseless
-- is returned, because paper suggests doing so.
classifyHeaders
    :: WorkMode ssc m
    => NonEmpty (BlockHeader ssc) -> m (ClassifyHeadersRes ssc)
classifyHeaders headers@(h:|hs) = do
    haveLast <- isJust <$> DB.getBlockHeader (hash $ NE.last headers)
    let headersValid = isVerSuccess $ verifyHeaders True $ h : hs
    if | not headersValid ->
             pure $ CHsInvalid "Header chain is invalid"
       | not haveLast ->
             pure $ CHsInvalid "Last block of the passed chain wasn't found locally"
       | otherwise -> processClassify
  where
    processClassify = do
        tipHeader <- view blockHeader <$> DB.getTipBlock
        lcaHash <- fromMaybe (panic "lca should exist") <$> lcaWithMainChain headers
        lca <- fromMaybe (panic "lca should be resolvable") <$> DB.getBlockHeader lcaHash
        -- depth in terms of slots, not difficulty
        let depthDiff =
                flattenEpochOrSlot tipHeader -
                flattenEpochOrSlot lca
        pure $ if
            | hash lca == hash tipHeader -> CHsValid tipHeader
            | depthDiff < 0 -> panic "classifyHeaders@depthDiff is negative"
            | depthDiff > k ->
                  CHsUseless $
                  sformat ("Slot difference of (tip,lca) is "%int%
                           " which is more than k = "%int)
                          depthDiff (k :: Int)
            | otherwise -> CHsValid lca

-- | Takes a starting header hash and queries blockchain until some
-- condition is true or parent wasn't found. Returns headers newest
-- first.
loadHeadersUntil
    :: forall ssc m.
       (MonadDB ssc m, Ssc ssc)
    => HeaderHash ssc
    -> (BlockHeader ssc -> Int -> Bool)
    -> m [BlockHeader ssc]
loadHeadersUntil startHHash cond = reverse <$> loadHeadersUntilDo startHHash 0
  where
    loadHeadersUntilDo :: HeaderHash ssc -> Int -> m [BlockHeader ssc]
    loadHeadersUntilDo curH depth = do
        curHeaderM <- DB.getBlockHeader curH
        let guarded' = curHeaderM >>= \v -> guard (cond v depth) >> pure v
        maybe (pure [])
              (\curHeader ->
                 (curHeader:) <$>
                 loadHeadersUntilDo (curHeader ^. prevBlockL ) (succ depth))
              guarded'

-- | Given a set of checkpoints to stop at, we take second header hash
-- block (or tip if latter is @Nothing@) and fetch the blocks until we
-- reach genesis block or one of checkpoints.
retrieveHeadersFromTo
    :: (MonadDB ssc m, Ssc ssc)
    => [HeaderHash ssc] -> Maybe (HeaderHash ssc) -> m [BlockHeader ssc]
retrieveHeadersFromTo checkpoints startM = do
    validCheckpoints <- catMaybes <$> mapM DB.getBlockHeader checkpoints
    tip <- DB.getTip
    let startFrom = fromMaybe tip startM
        neq = (/=) `on` getEpochOrSlot
        untilCond bh _ = all (neq bh) validCheckpoints
    headers <- loadHeadersUntil startFrom untilCond
    -- In case we didn't reach the very-first block we take one more
    -- because "until" predicate will stop us before we get
    -- checkpoint block and we do want to return it as well
    oneMore <- case headers of
        []       -> pure Nothing
        (last:_) -> DB.getBlockHeader $ last ^. prevBlockL
    pure $ reverse $ maybe identity (:) oneMore $ headers

-- | Given a starting point hash (we take tip if it's not in storage)
-- it returns not more than 'k' blocks distributed exponentially base
-- 2 relatively to the depth in the blockchain.
getHeadersOlderExp
    :: (MonadDB ssc m, Ssc ssc)
    => Maybe (HeaderHash ssc) -> m [HeaderHash ssc]
getHeadersOlderExp upto = do
    tip <- DB.getTip
    let upToReal = fromMaybe tip upto
        untilCond _ depth = depth <= k
    allHeaders <- loadHeadersUntil upToReal untilCond
    pure $ selectIndices (takeHashes allHeaders) twoPowers
  where
    -- Given list of headers newest first, maps it to their hashes
    takeHashes [] = []
    takeHashes headers@(x:_) =
        let prevHashes = map (view prevBlockL) headers
        in hash x : take (length prevHashes - 1) prevHashes
    -- Powers of 2
    twoPowers = (takeWhile (<k) $ 0 : 1 : iterate (*2) 2) ++ [k]
    -- Effectively do @!i@ for any @i@ from the index list applied to
    -- source list. Index list should be inreasing.
    selectIndices :: [a] -> [Int] -> [a]
    selectIndices elems ixs =
        let selGo _ [] _ = []
            selGo [] _ _ = []
            selGo ee@(e:es) ii@(i:is) skipped
                | skipped == i = e : selGo ee is skipped
                | otherwise    = selGo es ii $ succ skipped
        in selGo elems ixs 0

-- CHECK: @verifyBlocksLogic
-- | Verify blocks received from network. Head is expected to be the
-- oldest blocks. If parent of head is not our tip, verification
-- fails. This function checks everything from block, including
-- header, transactions, SSC data.
--
-- #txVerifyBlocks
verifyBlocks
    :: WorkMode ssc m
    => NonEmpty (Block ssc) -> m (Either Text (NonEmpty Undo))
verifyBlocks blocks =
    runExceptT $
    do curSlot <- getCurrentSlot
       verResToMonadError formatAllErrors $ Types.verifyBlocks (Just curSlot) blocks
       verResToMonadError formatAllErrors =<< sscVerifyBlocks False blocks
       ExceptT $ txVerifyBlocks blocks

-- | Run action acquiring lock on block application. Argument of
-- action is an old tip, result is put as a new tip.
withBlkSemaphore
    :: WorkMode ssc m
    => (HeaderHash ssc -> m (HeaderHash ssc)) -> m ()
withBlkSemaphore action = do
    tip <- takeBlkSemaphore
    let putBack = putBlkSemaphore tip
    onException (action tip >>= putBlkSemaphore) putBack

-- | Apply definitely valid sequence of blocks. At this point we must
-- have verified all predicates regarding block (including txs and ssc
-- data checks).  We almost must have taken lock on block application
-- and ensured that chain is based on our tip.
applyBlocks :: WorkMode ssc m => NonEmpty (Block ssc, Undo) -> m ()
applyBlocks blksUndos = do
    let blks = fmap fst blksUndos
    mapM_ putToDB blksUndos
    txApplyBlocks blks
    sscApplyBlocks blks
  where
    putToDB (blk, undo) = DB.putBlock undo True blk

-- | Rollback sequence of blocks, head block corresponds to tip,
-- further blocks are parents. It's assumed that lock on block
-- application is taken.
rollbackBlocks :: (WorkMode ssc m) => NonEmpty (Block ssc, Undo) -> m ()
rollbackBlocks toRollback = do
    -- [CSL-378] Update sbInMain
    txRollbackBlocks toRollback
    sscRollback $ fmap fst toRollback
