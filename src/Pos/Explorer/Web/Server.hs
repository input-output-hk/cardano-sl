{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ViewPatterns    #-}

-- API server logic

module Pos.Explorer.Web.Server
       ( explorerServeImpl
       , explorerApp
       , explorerHandlers
       ) where

import           Control.Monad.Catch            (try)
import           Control.Monad.Loops            (unfoldrM)
import           Control.Monad.Trans.Maybe      (MaybeT (..))
import           Data.Maybe                     (fromMaybe)
import           Network.Wai                    (Application)
import           Servant.API                    ((:<|>) ((:<|>)))
import           Servant.Server                 (Handler, Server, ServerT, serve)
import           Universum

import           Pos.Communication              (SendActions)
import           Pos.Crypto                     (WithHash (..), withHash)
import qualified Pos.DB                         as DB
import qualified Pos.DB.GState                  as GS
import           Pos.Slotting                   (MonadSlots (..), getSlotStart)
import           Pos.Ssc.GodTossing             (SscGodTossing)
import           Pos.Txp                        (getLocalTxs)
import           Pos.Types                      (Address (..), Tx, blockTxs, gbHeader,
                                                 gbhConsensus, mcdSlot, mkCoin,
                                                 prevBlockL, topsortTxs)
import           Pos.Util                       (maybeThrow)
import           Pos.Web                        (serveImpl)
import           Pos.WorkMode                   (WorkMode)

import           Pos.Explorer.Aeson.ClientTypes ()
import           Pos.Explorer.Web.Api           (ExplorerApi, explorerApi)
import           Pos.Explorer.Web.ClientTypes   (CAddress (..), CAddressSummary (..),
                                                 CBlockEntry (..), CBlockSummary (..),
                                                 CHash, CTxEntry (..), fromCAddress,
                                                 fromCHash, toBlockEntry, toBlockSummary,
                                                 toTxEntry)
import           Pos.Explorer.Web.Error         (ExplorerError (..))

----------------------------------------------------------------
-- Top level functionality
----------------------------------------------------------------

type ExplorerMode m = WorkMode SscGodTossing m

explorerServeImpl :: ExplorerMode m => m Application -> Word16 -> m ()
explorerServeImpl = serveImpl

explorerApp :: ExplorerMode m => m (Server ExplorerApi) -> m Application
explorerApp serv = serve explorerApi <$> serv

----------------------------------------------------------------
-- Handlers
----------------------------------------------------------------

explorerHandlers :: ExplorerMode m => SendActions m -> ServerT ExplorerApi m
explorerHandlers sendActions =
    catchExplorerError ... defaultLimit 10 getLastBlocks
    :<|>
    catchExplorerError ... defaultLimit 10 getLastTxs
    :<|>
    catchExplorerError . getBlockSummary
    :<|>
    catchExplorerError . getAddressSummary
  where
    catchExplorerError = try
    f ... g = (f .) . g

defaultLimit
    :: Word                 -- default limit (default offset is always 0)
    -> (Word -> Word -> a)  -- action to transform
    -> Maybe Word
    -> Maybe Word
    -> a
defaultLimit lim action mlim moff =
    action (fromMaybe lim mlim) (fromMaybe 0 moff)

getLastBlocks :: ExplorerMode m => Word -> Word -> m [CBlockEntry]
getLastBlocks lim off = do
    tip <- GS.getTip
    let getNextBlk h _ = fmap (view prevBlockL) $
            DB.getBlockHeader h >>=
            maybeThrow (Internal "Block database is malformed!")
    start <- foldlM getNextBlk tip [0..off]

    let unfolder n h = do
            when (n == 0) $
                fail "limit!"
            MaybeT (DB.getBlock h) >>= \case
                Left gb -> unfolder n (gb ^. prevBlockL)
                Right mb -> (,) <$> lift (toBlockEntry mb) <*>
                            pure (n - 1, mb ^. prevBlockL)
    flip unfoldrM (lim, start) $ \(n, h) -> runMaybeT $ unfolder n h

topsortTxsOrFail :: MonadThrow m => (a -> WithHash Tx) -> [a] -> m [a]
topsortTxsOrFail f =
    maybeThrow (Internal "Dependency loop in txs set") .
    topsortTxs f

getLastTxs :: ExplorerMode m => Word -> Word -> m [CTxEntry]
getLastTxs (fromIntegral -> lim) (fromIntegral -> off) = do
    let mkWhTx (txid, (tx, _, _)) = WithHash tx txid
    localTxs <- fmap reverse $ topsortTxsOrFail mkWhTx =<< getLocalTxs
    ts <- getCurrentTime

    let neededLocalTxs = take lim $ drop off localTxs
        localTxEntries = map (toTxEntry ts . view (_2 . _1)) neededLocalTxs
        lenTxs = length localTxs
        offLeft = off - lenTxs
        nLeft = offLeft + lim

    tip <- GS.getTip
    let unfolder off lim h = do
            when (lim <= 0) $
                fail "Finished"
            MaybeT (DB.getBlock h) >>= \case
                Left gb -> unfolder off lim (gb ^. prevBlockL)
                Right mb -> do
                    let mTxs = mb ^. blockTxs
                    if off >= length mTxs
                        then return ([], (off - length mTxs, lim, mb ^. prevBlockL))
                        else do
                        txs <- topsortTxsOrFail identity $ map withHash $ toList mTxs
                        blkSlotStart <- lift $ getSlotStart $
                                        mb ^. gbHeader . gbhConsensus . mcdSlot
                        let neededTxs = take lim $ drop off $ reverse txs
                            blkTxEntries = map (toTxEntry blkSlotStart . whData) neededTxs
                            offLeft = off - length mTxs
                            nLeft = offLeft + lim
                        return (blkTxEntries, (offLeft, nLeft, mb ^. prevBlockL))

    blockTxEntries <- fmap concat $ flip unfoldrM (offLeft, nLeft, tip) $
        \(o, l, h) -> runMaybeT $ unfolder o l h

    return $ localTxEntries ++ blockTxEntries

getBlockSummary :: ExplorerMode m => CHash -> m CBlockSummary
getBlockSummary (fromCHash -> h) = do
    blk <- maybeThrow (Internal "No block found") =<< DB.getBlock h
    case blk of
        Left db  -> throwM (Internal "Block is genesis block")
        Right mb -> toBlockSummary mb

getAddressSummary :: ExplorerMode m => CAddress -> m CAddressSummary
getAddressSummary cAddr = cAddrToAddr cAddr >>= \case
    PubKeyAddress sid -> do
        balance <- fromMaybe (mkCoin 0) <$> GS.getFtsStake sid
        -- TODO: add number of coins when it's implemented
        return $ CAddressSummary cAddr 0 balance
    _ -> throwM $
         Internal "Non-P2PKH addresses are not supported in Explorer yet"

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

cAddrToAddr :: MonadThrow m => CAddress -> m Address
cAddrToAddr cAddr =
    fromCAddress cAddr &
    either (\_ -> throwM $ Internal "Invalid address!") pure
