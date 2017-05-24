{-# LANGUAGE CPP                 #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

module Pos.Client.Txp.History
       ( TxHistoryEntry(..)
       , thTxId
       , thTx
       , thInputs
       , thDifficulty
       , thInputAddrs
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

import           Control.Lens                 (makeLenses, (%=))
import           Control.Monad.Loops          (unfoldrM)
import           Control.Monad.Trans          (MonadTrans)
import           Control.Monad.Trans.Identity (IdentityT (..))
import           Control.Monad.Trans.Maybe    (MaybeT (..))
import           Data.Coerce                  (coerce)
import qualified Data.DList                   as DL
import qualified Data.HashSet                 as HS
import           Data.Tagged                  (Tagged (..))
import qualified Ether
import           System.Wlog                  (WithLogger)

import           Pos.Block.Core               (Block, mainBlockTxPayload)
import           Pos.Constants                (blkSecurityParam)
import           Pos.Context.Context          (GenesisUtxo (..))
import           Pos.Core                     (Address, ChainDifficulty, HeaderHash,
                                               difficultyL, prevBlockL)
import           Pos.Crypto                   (WithHash (..), withHash)
import           Pos.DB                       (MonadDB, MonadDBPure)
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
                                               TxAux (..), TxDistribution, TxId, TxOut,
                                               TxOutAux (..), TxWitness, Utxo, UtxoStateT,
                                               applyTxToUtxo, evalUtxoStateT,
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
    , _thInputAddrs  :: ![Address]  -- TODO: remove in favor of _thInputs
    , _thOutputAddrs :: ![Address]
    } deriving (Show, Eq, Generic)

makeLenses ''TxHistoryEntry

-- | Type of monad used to deduce history
type TxSelectorT m = UtxoStateT (MaybeT m)

-- | Select transactions related to given addresses
getRelatedTxs
    :: Monad m
    => [Address]
    -> [(WithHash Tx, TxWitness, TxDistribution)]
    -> TxSelectorT m [TxHistoryEntry]
getRelatedTxs (HS.fromList -> addrs) txs = do
    lift (MaybeT $ return $ topsortTxs (view _1) txs) >>=
        fmap catMaybes . mapM step
  where
    step (WithHash tx txId, _wit, dist) = do
        inputs <- getSenders tx
        let outgoings = toList $ txOutAddress <$> _txOutputs tx
        let incomings = ordNub $ map txOutAddress inputs

        applyTxToUtxo (WithHash tx txId) dist

        return $ do
            guard . not . null $
                HS.fromList (incomings ++ outgoings) `HS.intersection` addrs
            return $ THEntry txId tx inputs Nothing incomings outgoings

-- | Given a full blockchain, derive address history and Utxo
-- TODO: Such functionality will still be useful for merging
-- blockchains when wallet state is ready, but some metadata for
-- Tx will be required.
deriveAddrHistory
    -- :: (Monad m, Ssc ssc) => Address -> [Block ssc] -> TxSelectorT m [TxHistoryEntry]
    :: (Monad m) => [Address] -> [Block ssc] -> TxSelectorT m [TxHistoryEntry]
deriveAddrHistory addr chain = do
    ether $ identity %= filterUtxoByAddrs addr
    deriveAddrHistoryPartial [] addr chain

deriveAddrHistoryPartial
    :: (Monad m)
    => [TxHistoryEntry]
    -> [Address]
    -> [Block ssc]
    -> TxSelectorT m [TxHistoryEntry]
deriveAddrHistoryPartial hist addrs chain =
    DL.toList <$> foldrM updateAll (DL.fromList hist) chain
  where
    updateAll (Left _) hst = pure hst
    updateAll (Right blk) hst = do
        let mapper TxAux {..} = (withHash taTx, taWitness, taDistribution)
        txs <- getRelatedTxs addrs $
                   map mapper $ flattenTxPayload (blk ^. mainBlockTxPayload)
        let difficulty = blk ^. difficultyL
            txs' = map (thDifficulty .~ Just difficulty) txs
        return $ DL.fromList txs' <> hst

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
    ( MonadDB m
    , MonadDBPure m
    , MonadThrow m
    , WithLogger m
    , MonadSlots m
    , Ether.MonadReader' GenesisUtxo m
    , MonadTxpMem TxpExtra_TMP m
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

        let blockFetcher h txs = do
                blk <- lift . lift . DB.runBlockDBRedirect $ DB.blkGetBlock @ssc h >>=
                       maybeThrow (DBMalformed "A block mysteriously disappeared!")
                deriveAddrHistoryPartial txs addrs [blk]
            localFetcher blkTxs = do
                let mp (txid, TxAux {..}) =
                      (WithHash taTx txid, taWitness, taDistribution)
                ltxs <- lift . lift $ getLocalTxs
                txs <- getRelatedTxs addrs $ map mp ltxs
                return $ txs ++ blkTxs

        mres <- runMaybeT $ do
            (cachedTxs, cachedUtxo) <- runUtxoStateT
                (foldrM blockFetcher [] cachedHashes) genUtxo

            result <- evalUtxoStateT
                (foldrM blockFetcher cachedTxs nonCachedHashes >>= localFetcher)
                cachedUtxo

            let lastCachedHash = fromMaybe bot $ head cachedHashes
            return $ TxHistoryAnswer lastCachedHash (length cachedTxs) cachedUtxo result

        maybe (error "deriveAddrHistory: Nothing") pure mres

#ifdef WITH_EXPLORER
    saveTx txw = () <$ runExceptT (eTxProcessTransaction txw)
#else
    saveTx txw = () <$ runExceptT (txProcessTransaction txw)
#endif
