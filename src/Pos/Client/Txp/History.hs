{-# LANGUAGE CPP                 #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

module Pos.Client.Txp.History
       ( TxHistoryEntry(..)
       , _thInputAddrs
       , thTxId
       , thTx
       , thInputs
       , thDifficulty
       , thOutputAddrs

       , TxHistoryAnswer(..)

       , MonadTxHistory(..)

       -- * History derivation
       , getRelatedTxs
       , deriveAddrHistory
       , deriveAddrHistoryPartial
       , TxHistoryRedirect
       , runTxHistoryRedirect
       ) where

import           Universum

import           Control.Lens                 (ix, makeLenses, (%=), (<>=))
import           Control.Monad.Loops          (unfoldrM)
import           Control.Monad.Trans          (MonadTrans)
import           Control.Monad.Trans.Control  (MonadBaseControl)
import           Control.Monad.Trans.Identity (IdentityT (..))
import           Control.Monad.Trans.Maybe    (MaybeT (..))
import           Data.Coerce                  (coerce)
import qualified Data.HashSet                 as HS
import qualified Data.Map                     as M
import           Data.Tagged                  (Tagged (..))
import qualified Ether
import           System.Wlog                  (WithLogger)

import           Pos.Block.Core               (Block, mainBlockTxPayload)
import           Pos.Constants                (blkSecurityParam)
import           Pos.Context.Context          (GenesisUtxo (..))
import           Pos.Core                     (Address, ChainDifficulty, HeaderHash,
                                               difficultyL, prevBlockL)
import           Pos.Crypto                   (WithHash (..), withHash)
import           Pos.DB                       (MonadDBRead, MonadRealDB)
import qualified Pos.DB.Block                 as DB
import           Pos.DB.Error                 (DBError (..))
import qualified Pos.DB.GState                as GS
import           Pos.Slotting                 (MonadSlots)
import           Pos.Ssc.Class                (SscHelpersClass)
#ifdef WITH_EXPLORER
import           Pos.Explorer.Txp.Local       (eTxProcessTransaction)
#else
import           Pos.Txp                      (txProcessTransaction)
#endif
import           Pos.Txp                      (MonadTxpMem, MonadUtxoRead, Tx (..),
                                               TxAux (..), TxDistribution, TxId, TxIn,
                                               TxOut, TxOutAux (..), TxWitness, Utxo,
                                               UtxoStateT, applyTxToUtxo,
                                               applyTxToUtxoPure, evalUtxoStateT,
                                               filterUtxoByAddrs, flattenTxPayload,
                                               getLocalTxs, runUtxoStateT, topsortTxs,
                                               txOutAddress, utxoGet)
import           Pos.Util                     (ether, maybeThrow)
import           Pos.WorkMode.Class           (TxpExtra_TMP)

-- Remove this once there's no #ifdef-ed Pos.Txp import
{-# ANN module ("HLint: ignore Use fewer imports" :: Text) #-}

data TxHistoryAnswer = TxHistoryAnswer
    { taLastCachedHash :: HeaderHash
    , taCachedNum      :: Int
    , taCachedUtxo     :: Utxo
    , taHistory        :: [TxHistoryEntry]
    } deriving (Show)

----------------------------------------------------------------------
-- Deduction of history
----------------------------------------------------------------------

-- | For given tx, gives list of source addresses of this tx, with respective 'TxIn's
getSenders :: MonadUtxoRead m => Tx -> m [TxOut]
getSenders UnsafeTx {..} = do
    utxo <- catMaybes <$> mapM utxoGet (toList _txInputs)
    return $ toaOut <$> utxo

-- | Datatype for returning info about tx history
data TxHistoryEntry = THEntry
    { _thTxId        :: !TxId
    , _thTx          :: !Tx
    , _thInputs      :: ![TxOut]
    , _thDifficulty  :: !(Maybe ChainDifficulty)
    , _thOutputAddrs :: ![Address]
    } deriving (Show, Eq, Generic)

makeLenses ''TxHistoryEntry

_thInputAddrs :: TxHistoryEntry -> [Address]
_thInputAddrs = map txOutAddress . _thInputs


-- | Type of monad used to deduce history
type TxSelectorT m = UtxoStateT (MaybeT m)

-- | Select transactions related to given addresses
getRelatedTxs
    :: Monad m
    => [Address]
    -> [(WithHash Tx, TxWitness, TxDistribution)]
    -> TxSelectorT m (Map TxIn TxHistoryEntry)
getRelatedTxs addrs txs = do
    lift (MaybeT $ return $ topsortTxs (view _1) txs) >>=
        fmap (mconcat . catMaybes) . mapM step
  where
    step (WithHash tx txId, _wit, dist) = do
        inputs <- getSenders tx
        let outgoings = toList $ txOutAddress <$> _txOutputs tx
        let incomings = ordNub $ map txOutAddress inputs

        runMaybeT $ do
            guard . not . null $
                HS.fromList (incomings ++ outgoings) `HS.intersection` addrsSet

            lift $ applyTxToUtxo (WithHash tx txId) dist
            lift $ ether $ identity %= filterUtxoByAddrs addrs

            -- '_thInputs' to be completed later
            return . M.fromList $
                map (, THEntry txId tx inputs Nothing outgoings) (toList $ _txInputs tx)
    addrsSet = HS.fromList addrs

type THInputsFillerT m = Ether.StateT' (Map TxIn TxHistoryEntry) (MaybeT m)

fillTxsInputs
    :: Monad m
    => [(WithHash Tx, TxWitness, TxDistribution)]
    -> THInputsFillerT m ()
fillTxsInputs txs = do
    lift (MaybeT $ return $ topsortTxs (view _1) txs) >>= traverse_ step
  where
    step
        :: Monad m
        => (WithHash Tx, TxWitness, TxDistribution) -> THInputsFillerT m ()
    step (WithHash tx txId, _wit, dist) = do
        let utxoDelta = applyTxToUtxoPure (WithHash tx txId) dist mempty
        forM_ (M.toList utxoDelta) $ \(txIn, TxOutAux txOut _) ->
            ether $ ix txIn . thInputs <>= [txOut]

-- | Given a full blockchain, derive address history and Utxo
-- TODO: Such functionality will still be useful for merging
-- blockchains when wallet state is ready, but some metadata for
-- Tx will be required.
deriveAddrHistory
    -- :: (Monad m, Ssc ssc) => Address -> [Block ssc] -> TxSelectorT m [TxHistoryEntry]
    :: (Monad m) => [Address] -> [Block ssc] -> TxSelectorT m [TxHistoryEntry]
deriveAddrHistory addrs chain = do
    ether $ identity %= filterUtxoByAddrs addrs
    toList <$> deriveAddrHistoryPartial addrs mempty chain

deriveAddrHistoryPartial
    :: (Monad m)
    => [Address]
    -> Map TxIn TxHistoryEntry
    -> [Block ssc]
    -> TxSelectorT m (Map TxIn TxHistoryEntry)
deriveAddrHistoryPartial addrs hist chain =
    foldrM updateAll hist chain
  where
    updateAll (Left _) hst = pure hst
    updateAll (Right blk) hst = do
        let mapper TxAux {..} = (withHash taTx, taWitness, taDistribution)
        txs <- getRelatedTxs addrs $
                   map mapper $ flattenTxPayload (blk ^. mainBlockTxPayload)
        let difficulty = blk ^. difficultyL
            txs' = fmap (thDifficulty .~ Just difficulty) txs
        return $ txs' <> hst

-- TODO: remove boilerplate
fillBlocksTxsInputs
    :: (Monad m)
    => [Block ssc]
    -> THInputsFillerT m ()
fillBlocksTxsInputs chain =
    mapM_ updateAll chain
  where
    updateAll (Left _) = return ()
    updateAll (Right blk) = do
        let mapper TxAux {..} = (withHash taTx, taWitness, taDistribution)
        fillTxsInputs $ map mapper $ flattenTxPayload (blk ^. mainBlockTxPayload)

----------------------------------------------------------------------------
-- MonadTxHistory
----------------------------------------------------------------------------

-- | A class which have methods to get transaction history
class Monad m => MonadTxHistory m where
    getTxHistory
        :: SscHelpersClass ssc
        => Tagged ssc ([Address] -> Maybe (HeaderHash, Utxo) -> m TxHistoryAnswer)
    saveTx :: (TxId, TxAux) -> m ()

    default getTxHistory
        :: (SscHelpersClass ssc, MonadTrans t, MonadTxHistory m', t m' ~ m)
        => Tagged ssc ([Address] -> Maybe (HeaderHash, Utxo) -> m TxHistoryAnswer)
    getTxHistory = fmap lift <<$>> getTxHistory

    default saveTx :: (MonadTrans t, MonadTxHistory m', t m' ~ m) => (TxId, TxAux) -> m ()
    saveTx = lift . saveTx

instance {-# OVERLAPPABLE #-}
    (MonadTxHistory m, MonadTrans t, Monad (t m)) =>
        MonadTxHistory (t m)

data TxHistoryRedirectTag

type TxHistoryRedirect =
    Ether.TaggedTrans TxHistoryRedirectTag IdentityT

runTxHistoryRedirect :: TxHistoryRedirect m a -> m a
runTxHistoryRedirect = coerce

instance
    ( MonadRealDB m
    , MonadDBRead m
    , MonadThrow m
    , WithLogger m
    , MonadSlots m
    , Ether.MonadReader' GenesisUtxo m
    , MonadTxpMem TxpExtra_TMP m
    , MonadBaseControl IO m
    , t ~ IdentityT
    ) => MonadTxHistory (Ether.TaggedTrans TxHistoryRedirectTag t m)
  where
    getTxHistory :: forall ssc. SscHelpersClass ssc
                 => Tagged ssc ([Address] -> Maybe (HeaderHash, Utxo) -> TxHistoryRedirect m TxHistoryAnswer)
    getTxHistory = Tagged $ \addrs mInit -> do
        tip <- GS.getTip

        let getGenUtxo = Ether.asks' unGenesisUtxo
        (bot, genUtxo) <- maybe ((,) <$> GS.getBot <*> getGenUtxo) pure mInit

        -- Getting list of all hashes in main blockchain (excluding bottom block - it's genesis anyway)
        hashList <- flip unfoldrM tip $ \h ->
            if h == bot
            then return Nothing
            else do
                header <- DB.runBlockDBRedirect $ DB.blkGetHeader @ssc h >>=
                    maybeThrow (DBMalformed "Best blockchain is non-continuous")
                let prev = header ^. prevBlockL
                return $ Just (h, prev)

        -- Determine last block which txs should be cached
        let cachedHashes = drop blkSecurityParam hashList
            nonCachedHashes = take blkSecurityParam hashList

        let blockFetcher
                :: (MonadRealDB n, MonadDBRead n, MonadThrow n)
                => ([Block ssc] -> n a)
                -> HeaderHash
                -> n a
            blockFetcher takeRes h = do
                blk <- DB.runBlockDBRedirect $ DB.blkGetBlock @ssc h >>=
                       maybeThrow (DBMalformed "A block mysteriously disappeared!")
                takeRes [blk]
            localFetcher
                :: (MonadIO n, MonadTxpMem TxpExtra_TMP n, Monoid a)
                => ([(WithHash Tx, TxWitness, TxDistribution)] -> n a)
                -> a
                -> n a
            localFetcher takeRes blkTxs = do
                let mp (txid, TxAux {..}) =
                      (WithHash taTx txid, taWitness, taDistribution)
                ltxs <- getLocalTxs
                txs <- takeRes $ map mp ltxs
                return $ txs <> blkTxs

        mres <- runMaybeT $ do
            let blockHistoryFetcher h txs =
                    blockFetcher (deriveAddrHistoryPartial addrs txs) h
            (cachedTxs, cachedUtxo) <- runUtxoStateT
                 (foldrM blockHistoryFetcher mempty cachedHashes)
                 genUtxo

            thPart <- evalUtxoStateT
                (foldrM blockHistoryFetcher cachedTxs nonCachedHashes
                     >>= localFetcher (getRelatedTxs addrs))
                cachedUtxo

            let blockHistoryFiller h _ =
                    blockFetcher fillBlocksTxsInputs h
            thFull <- flip Ether.execStateT thPart $
                 foldrM blockHistoryFiller () hashList
                         >>= localFetcher (fillTxsInputs)

            let lastCachedHash = fromMaybe bot $ head cachedHashes
            let result = toList thFull
            return $ TxHistoryAnswer lastCachedHash (length cachedTxs) cachedUtxo result

        maybe (error "deriveAddrHistory: Nothing") pure mres

#ifdef WITH_EXPLORER
    saveTx txw = () <$ runExceptT (eTxProcessTransaction txw)
#else
    saveTx txw = () <$ runExceptT (txProcessTransaction txw)
#endif
