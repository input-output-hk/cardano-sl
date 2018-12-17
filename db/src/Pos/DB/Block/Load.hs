-- | Loading sequence of blunds.

module Pos.DB.Block.Load
       (
       -- * Load data
         loadBlundsWhile
       , loadBlundsByDepth
       , loadBlocksWhile
       , loadHeadersWhile
       , loadHeadersByDepth

       -- * Load data from tip
       , loadBlundsFromTipWhile
       , loadBlundsFromTipByDepth

       , getUndo
       , getBlund
       , putBlunds
       , getTipBlock
       ) where

import           Universum

import           Control.Lens (_Wrapped)
import           Formatting (sformat, (%))

import           Pos.Binary.Class (serialize')
import           Pos.Chain.Block (Block, BlockHeader, Blund, HasPrevBlock (..),
                     HeaderHash, Undo (..))
import qualified Pos.Chain.Block as CB
import           Pos.Chain.Genesis (GenesisHash (..))
import           Pos.Core (BlockCount, HasDifficulty (difficultyL))
import           Pos.Core.Chrono (NewestFirst (..))
import           Pos.Crypto (shortHashF)
import           Pos.DB.BlockIndex (getHeader)
import           Pos.DB.Class (MonadBlockDBRead, MonadDB (..), MonadDBRead (..),
                     Serialized (..), getBlock, getDeserialized)
import           Pos.DB.Error (DBError (..))
import           Pos.DB.GState.Common (getTip, getTipSomething)
import           Pos.Util.Util (maybeThrow)

type LoadHeadersMode m =
    ( MonadDBRead m
    )

----------------------------------------------------------------------------
-- Load
----------------------------------------------------------------------------

loadDataWhile
    :: forall m a .
       (Monad m, HasPrevBlock a)
    => GenesisHash
    -> (HeaderHash -> m a)
    -> (a -> Bool)
    -> HeaderHash
    -> m (NewestFirst [] a)
loadDataWhile genesisHash getter predicate start = NewestFirst <$> doIt [] start
  where
    doIt :: [a] -> HeaderHash -> m [a]
    doIt !acc h
        | h == getGenesisHash genesisHash = pure (reverse acc)
        | otherwise = do
            d <- getter h
            let prev = d ^. prevBlockL
            if predicate d
                then doIt (d : acc) prev
                else pure (reverse acc)

-- For depth 'd' load blocks that have depth < 'd'. Given header
-- (newest one) is assumed to have depth 0.
loadDataByDepth
    :: forall m a .
       (Monad m, HasPrevBlock a, HasDifficulty a)
    => GenesisHash
    -> (HeaderHash -> m a)
    -> (a -> Bool)
    -> BlockCount
    -> HeaderHash
    -> m (NewestFirst [] a)
loadDataByDepth _ _ _ 0 _ = pure (NewestFirst [])
loadDataByDepth genesisHash getter extraPredicate depth h = do
    -- First of all, we load data corresponding to h.
    top <- getter h
    let topDifficulty = top ^. difficultyL
    -- If top difficulty is 0, we can load all data starting from it.
    -- Then we calculate difficulty of data at which we should stop.
    -- Difficulty of the oldest data to return is 'topDifficulty - depth + 1'
    -- So we are loading all blocks which have difficulty ≥ targetDifficulty.
    let targetDelta = fromIntegral depth - 1
        targetDifficulty
            | topDifficulty <= targetDelta = 0
            | otherwise = topDifficulty - targetDelta
    -- Then we load blocks starting with previous block of already
    -- loaded block.  We load them until we find block with target
    -- difficulty. And then we drop last (oldest) block.
    let prev = top ^. prevBlockL
    over _Wrapped (top :) <$>
        loadDataWhile
        genesisHash
        getter
        (\a -> a ^. difficultyL >= targetDifficulty && extraPredicate a)
        prev

-- | Load blunds starting from block with header hash equal to given hash
-- and while @predicate@ is true.
loadBlundsWhile
    :: MonadDBRead m
    => GenesisHash -> (Block -> Bool) -> HeaderHash -> m (NewestFirst [] Blund)
loadBlundsWhile genesisHash predicate =
    loadDataWhile genesisHash (getBlundThrow genesisHash) (predicate . fst)

-- | Load blunds which have depth less than given (depth = number of
-- blocks that will be returned).
loadBlundsByDepth
    :: MonadDBRead m
    => GenesisHash
    -> BlockCount
    -> HeaderHash
    -> m (NewestFirst [] Blund)
loadBlundsByDepth genesisHash =
    loadDataByDepth genesisHash (getBlundThrow genesisHash) (const True)

-- | Load blocks starting from block with header hash equal to given hash
-- and while @predicate@ is true.
loadBlocksWhile
    :: MonadBlockDBRead m
    => GenesisHash -> (Block -> Bool) -> HeaderHash -> m (NewestFirst [] Block)
loadBlocksWhile genesisHash =
    loadDataWhile genesisHash $ getBlockThrow genesisHash

-- | Load headers starting from block with header hash equal to given hash
-- and while @predicate@ is true.
loadHeadersWhile
    :: LoadHeadersMode m
    => GenesisHash
    -> (BlockHeader -> Bool)
    -> HeaderHash
    -> m (NewestFirst [] BlockHeader)
loadHeadersWhile genesisHash = loadDataWhile genesisHash getHeaderThrow

-- | Load headers which have depth less than given.
loadHeadersByDepth
    :: LoadHeadersMode m
    => GenesisHash -> BlockCount -> HeaderHash -> m (NewestFirst [] BlockHeader)
loadHeadersByDepth genesisHash =
    loadDataByDepth genesisHash getHeaderThrow (const True)

----------------------------------------------------------------------------
-- Load from tip
----------------------------------------------------------------------------

-- | Load blunds from BlockDB starting from tip and while the @condition@ is
-- true.
loadBlundsFromTipWhile
    :: MonadDBRead m
    => GenesisHash -> (Block -> Bool) -> m (NewestFirst [] Blund)
loadBlundsFromTipWhile genesisHash condition =
    getTip >>= loadBlundsWhile genesisHash condition

-- | Load blunds from BlockDB starting from tip which have depth less than
-- given.
loadBlundsFromTipByDepth
    :: MonadDBRead m
    => GenesisHash -> BlockCount -> m (NewestFirst [] Blund)
loadBlundsFromTipByDepth genesisHash d =
    getTip >>= loadBlundsByDepth genesisHash d

----------------------------------------------------------------------------
-- Private functions
----------------------------------------------------------------------------

getBlockThrow
    :: MonadBlockDBRead m
    => GenesisHash -> HeaderHash -> m Block
getBlockThrow genesisHash hash =
    maybeThrow (DBMalformed $ sformat errFmt hash) =<< getBlock genesisHash hash
  where
    errFmt = "getBlockThrow: no block with HeaderHash: "%shortHashF

getHeaderThrow
    :: LoadHeadersMode m
    => HeaderHash -> m BlockHeader
getHeaderThrow hash =
    maybeThrow (DBMalformed $ sformat errFmt hash) =<< getHeader hash
  where
    errFmt = "getBlockThrow: no block header with hash: "%shortHashF

getBlundThrow
    :: MonadDBRead m
    => GenesisHash -> HeaderHash -> m Blund
getBlundThrow genesisHash hash =
    maybeThrow (DBMalformed $ sformat errFmt hash) =<< getBlund genesisHash hash
  where
    errFmt = "getBlundThrow: no blund with HeaderHash: "%shortHashF

----------------------------------------------------------------------------
-- BlockDB related methods
----------------------------------------------------------------------------

getUndo :: MonadDBRead m => GenesisHash -> HeaderHash -> m (Maybe Undo)
getUndo = getDeserialized . dbGetSerUndo

-- | Convenient wrapper which combines 'dbGetBlock' and 'dbGetUndo' to
-- read 'Blund'.
--
-- TODO Rewrite to use a single call
getBlund :: MonadDBRead m => GenesisHash -> HeaderHash -> m (Maybe (Block, Undo))
getBlund genesisHash x =
    runMaybeT $
    (,) <$> MaybeT (getBlock genesisHash x)
        <*> MaybeT (getUndo genesisHash x)

-- | Store blunds into a single file.
--
--   Notice that this uses an unusual encoding, in order to be able to fetch
--   either the block or the undo independently without re-encoding.
putBlunds :: MonadDB m => NonEmpty Blund -> m ()
putBlunds = dbPutSerBlunds
          . map (\bu@(b,_) -> ( CB.getBlockHeader b
                              , Serialized . serialize' $ bimap serialize' serialize' bu)
                )

-- | Get 'Block' corresponding to tip.
getTipBlock :: MonadDBRead m => GenesisHash -> m Block
getTipBlock = getTipSomething "block" . getBlock
