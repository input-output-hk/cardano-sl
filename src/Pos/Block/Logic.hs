{-# LANGUAGE FlexibleContexts #-}

-- | Logic of blocks processing.

module Pos.Block.Logic
       ( ClassifyHeaderRes (..)
       , applyBlocks
       , classifyNewHeader
       , classifyHeaders
       , rollbackBlocks
       , verifyBlocks
       , withBlkSemaphore
       ) where

import           Control.Lens         ((^.))
import           Control.Monad.Catch  (onException)
import           Data.Default         (Default (def))
import           Data.List.NonEmpty   (NonEmpty)
import qualified Data.Text            as T
import           Serokell.Util.Verify (VerificationRes (..))
import           Universum

import           Pos.Context          (putBlkSemaphore, takeBlkSemaphore)
import qualified Pos.Modern.DB        as DB
import           Pos.Modern.Txp.Logic (txApplyBlocks, txRollbackBlocks, txVerifyBlocks)
import           Pos.Slotting         (getCurrentSlot)
import           Pos.Types            (Block, BlockHeader, HeaderHash, Undo,
                                       VerifyHeaderParams (..), blockHeader, difficultyL,
                                       headerSlot, prevBlockL, verifyHeader,
                                       vhpVerifyConsensus)
import           Pos.Util             (eitherToVerRes)
import           Pos.WorkMode         (WorkMode)

-- | Result of header classification.
data ClassifyHeaderRes
    = CHRcontinues      -- ^ Header continues our main chain.
    | CHRalternative    -- ^ Header continues alternative chain which
                        -- is more difficult.
    | CHRuseless !Text  -- ^ Header is useless.
    | CHRinvalid !Text  -- ^ Header is invalid.

-- | Make `ClassifyHeaderRes` from list of error messages using
-- `CHRinvalid` constructor. Intended to be used with `VerificationRes`.
-- Note: this version forces computation of all error messages. It can be
-- made more efficient but less informative by using head, for example.
mkCHRinvalid :: [Text] -> ClassifyHeaderRes
mkCHRinvalid = CHRinvalid . T.intercalate "; "

-- | Classify new header announced by some node. Result is represented
-- as ClassifyHeaderRes type.
classifyNewHeader
    :: (WorkMode ssc m)
    => BlockHeader ssc -> m ClassifyHeaderRes
-- Genesis headers seem useless, we can create them by ourselves.
classifyNewHeader (Left _) = pure $ CHRuseless "genesis header is useless"
classifyNewHeader (Right header) = do
    curSlot <- getCurrentSlot
    -- First of all we check whether header is from current slot and
    -- ignore it if it's not.
    if curSlot == header ^. headerSlot
        then classifyNewHeaderDo <$> DB.getTip <*> DB.getTipBlock
        else return $ CHRuseless "header is not for current slot"
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
                   VerSuccess        -> CHRcontinues
                   VerFailure errors -> mkCHRinvalid errors
        -- If header's parent is not our tip, we check whether it's
        -- more difficult than our main chain.
        | tipBlock ^. difficultyL < header ^. difficultyL = CHRalternative
        -- If header can't continue main chain and is not more
        -- difficult than main chain, it's useless.
        | otherwise =
            CHRuseless $
            "header doesn't continue main chain and is not more difficult"

-- | Classify headers received in response to 'GetHeaders' message.
-- • If there are any errors in chain of headers, CHRinvalid is returned.
-- • If chain of headers is a valid continuation of our main chain,
-- CHRcontinues is returned.
-- • If chain of headers forks from our main chain but not too much,
-- CHRalternative is returned.
-- • If chain of headers forks from our main chain too much, CHRuseless
-- is returned, because paper suggests doing so.
classifyHeaders
    :: WorkMode ssc m
    => [BlockHeader ssc] -> m ClassifyHeaderRes
classifyHeaders = notImplemented

-- CHECK: @verifyBlocksLogic
-- | Verify blocks received from network. Head is expected to be the
-- oldest blocks. If parent of head is not our tip, verification
-- fails. This function checks everything from block, including
-- header, transactions, SSC data.
--
-- #txVerifyBlocks
verifyBlocks
    :: WorkMode ssc m
    => NonEmpty (Block ssc) -> m VerificationRes
verifyBlocks blocks = do
    txsVerRes <- txVerifyBlocks blocks
    -- TODO: more checks of course. Consider doing CSL-39 first.
    return $ eitherToVerRes txsVerRes

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
applyBlocks :: WorkMode ssc m => NonEmpty (Block ssc) -> m ()
applyBlocks = mapM_ applyBlock

applyBlock :: (WorkMode ssc m) => Block ssc -> m ()
applyBlock blk = do
    -- [CSL-331] Put actual Undo instead of empty list!
    DB.putBlock [] True blk
    txApplyBlocks (pure blk)
    -- TODO: apply to SSC, maybe something else.

-- | Rollback sequence of blocks, head block corresponds to tip,
-- further blocks are parents. It's assumed that lock on block
-- application is taken.
rollbackBlocks :: (WorkMode ssc m) => NonEmpty (Block ssc, Undo) -> m ()
rollbackBlocks toRollback = do
    -- TODO: rollback SSC, maybe something else.
    txRollbackBlocks toRollback
