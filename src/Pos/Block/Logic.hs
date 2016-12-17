{-# LANGUAGE FlexibleContexts #-}

-- | Logic of blocks processing.

module Pos.Block.Logic
       ( verifyBlock
       , applyBlock
       ) where

import           Control.Lens           ((^.))
import           Control.Monad.Catch    (onException)
import           Universum

import           Pos.Context            (putBlkSemaphore, takeBlkSemaphore)
import qualified Pos.Modern.DB          as DB
import           Pos.Modern.Txp.Storage (txApplyBlocks)
import           Pos.Types              (Block, prevBlockL)
import           Pos.WorkMode           (WorkMode)

verifyBlock
    :: (WorkMode ssc m)
    => Block ssc -> m ()
verifyBlock _ = pass

-- | Apply definitely valid block. At this point we must have verified
-- all predicates regarding block (including txs and ssc data checks).
-- This function takes lock on block application and releases it after
-- finishing. It can fail if previous block of applied block differs
-- from stored tip.
--
-- TODO: this function should be more complex and most likely take at
-- least list of blocks.
applyBlock :: (WorkMode ssc m) => Block ssc -> m Bool
applyBlock blk = do
    tip <- takeBlkSemaphore
    let putBack = putBlkSemaphore tip
    if blk ^. prevBlockL == tip
        then True <$ onException (applyBlockDo blk) putBack
        else False <$ putBack

applyBlockDo :: (WorkMode ssc m) => Block ssc -> m ()
applyBlockDo blk = do
    DB.putBlock True blk
    txApplyBlocks (pure blk)
