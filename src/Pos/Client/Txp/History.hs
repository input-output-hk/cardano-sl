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
       , thTimestamp

       , MonadTxHistory(..)

       -- * History derivation
       , getRelatedTxsByAddrs
       , deriveAddrHistory
       , deriveAddrHistoryBlk
       , TxHistoryRedirect
       , runTxHistoryRedirect
       ) where

import           Universum

import           Control.Lens                 (makeLenses)
import           Control.Monad.Trans          (MonadTrans)
import           Control.Monad.Trans.Control  (MonadBaseControl)
import           Control.Monad.Trans.Identity (IdentityT (..))
import           Data.Coerce                  (coerce)
import           Data.DList                   (DList)
import qualified Data.DList                   as DL
import qualified Data.HashMap.Strict          as HM
import qualified Data.Map.Strict              as M (lookup)
import           Data.Tagged                  (Tagged (..))
import qualified Ether
import           System.Wlog                  (WithLogger)

import           Pos.Block.Core               (Block, MainBlock, mainBlockSlot,
                                               mainBlockTxPayload)
import           Pos.Block.Types              (Blund)
import           Pos.Context                  (GenesisUtxo, genesisUtxoM)
import           Pos.Core                     (Address, ChainDifficulty, HeaderHash,
                                               Timestamp (..), difficultyL)
import           Pos.Crypto                   (WithHash (..), withHash)
import           Pos.DB                       (MonadDBRead, MonadRealDB)
import qualified Pos.DB.Block                 as DB
import qualified Pos.DB.GState                as GS
import           Pos.Slotting                 (MonadSlots, getSlotStartPure)
import           Pos.Ssc.Class                (SscHelpersClass)
#ifdef WITH_EXPLORER
import           Pos.Explorer.Txp.Local       (eTxProcessTransaction)
#else
import           Pos.Txp                      (txProcessTransaction)
#endif
import           Pos.Txp                      (MonadTxpMem, MonadUtxo, MonadUtxoRead,
                                               ToilT, Tx (..), TxAux (..), TxDistribution,
                                               TxId, TxOut, TxOutAux (..), TxWitness,
                                               TxpError (..), applyTxToUtxo,
                                               evalToilTEmpty, flattenTxPayload,
                                               getLocalTxs, runDBTxp, topsortTxs,
                                               txOutAddress, utxoGet)
import           Pos.Util                     (maybeThrow)
import           Pos.WorkMode.Class           (TxpExtra_TMP)

-- Remove this once there's no #ifdef-ed Pos.Txp import
{-# ANN module ("HLint: ignore Use fewer imports" :: Text) #-}

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
    , _thTimestamp   :: !(Maybe Timestamp)
    } deriving (Show, Eq, Generic)

makeLenses ''TxHistoryEntry

-- | Select transactions by predicate on related addresses
getTxsByPredicate
    :: MonadUtxo m
    => ([Address] -> Bool)
    -> Maybe ChainDifficulty
    -> Maybe Timestamp
    -> [(WithHash Tx, TxWitness, TxDistribution)]
    -> m [TxHistoryEntry]
getTxsByPredicate pr mDiff mTs txs = go txs []
  where
    go [] acc = return acc
    go ((wh@(WithHash tx txId), _wit, dist) : rest) acc = do
        inputs <- getSenders tx
        let outgoings = toList $ txOutAddress <$> _txOutputs tx
        let incomings = ordNub $ map txOutAddress inputs

        applyTxToUtxo wh dist

        let acc' = if pr (incomings ++ outgoings)
                   then (THEntry txId tx inputs mDiff incomings outgoings mTs : acc)
                   else acc
        go rest acc'

-- | Select transactions related to one of given addresses
getRelatedTxsByAddrs
    :: MonadUtxo m
    => [Address]
    -> Maybe ChainDifficulty
    -> Maybe Timestamp
    -> [(WithHash Tx, TxWitness, TxDistribution)]
    -> m [TxHistoryEntry]
getRelatedTxsByAddrs addrs = getTxsByPredicate $ any (`elem` addrs)

-- | Given a full blockchain, derive address history and Utxo
-- TODO: Such functionality will still be useful for merging
-- blockchains when wallet state is ready, but some metadata for
-- Tx will be required.
deriveAddrHistory
    :: MonadUtxo m
    => [Address] -> [Block ssc] -> m [TxHistoryEntry]
deriveAddrHistory addrs chain =
    DL.toList <$> foldrM (flip $ deriveAddrHistoryBlk addrs $ const Nothing) mempty chain

deriveAddrHistoryBlk
    :: MonadUtxo m
    => [Address]
    -> (MainBlock ssc -> Maybe Timestamp)
    -> DList TxHistoryEntry
    -> Block ssc
    -> m (DList TxHistoryEntry)
deriveAddrHistoryBlk _ _ hist (Left _) = pure hist
deriveAddrHistoryBlk addrs getTs hist (Right blk) = do
    let mapper TxAux {..} = (withHash taTx, taWitness, taDistribution)
        difficulty = blk ^. difficultyL
        mTimestamp = getTs blk
    txs <- getRelatedTxsByAddrs addrs (Just difficulty) mTimestamp $
           map mapper . flattenTxPayload $
           blk ^. mainBlockTxPayload
    return $ DL.fromList txs <> hist

----------------------------------------------------------------------------
-- GenesisToil
----------------------------------------------------------------------------

-- | Identity wrapper to use genesis utxo in context as `MonadUtxoRead` instance
-- TODO: probably should be moved elsewhere; `Pos.Txp.Toil` is not possible, because
-- of dependency on `Pos.Context` from main package
data GenesisToilTag

type GenesisToil = Ether.TaggedTrans GenesisToilTag IdentityT

runGenesisToil :: GenesisToil m a -> m a
runGenesisToil = coerce

instance (Ether.MonadReader' GenesisUtxo m) =>
         MonadUtxoRead (GenesisToil m) where
    utxoGet txIn = M.lookup txIn <$> genesisUtxoM

----------------------------------------------------------------------------
-- MonadTxHistory
----------------------------------------------------------------------------

-- | A class which have methods to get transaction history
class Monad m => MonadTxHistory m where
    getBlockHistory
        :: SscHelpersClass ssc
        => Tagged ssc ([Address] -> m (DList TxHistoryEntry))
    getLocalHistory
        :: [Address] -> m (DList TxHistoryEntry)
    saveTx :: (TxId, TxAux) -> m ()

    default getBlockHistory
        :: (SscHelpersClass ssc, MonadTrans t, MonadTxHistory m', t m' ~ m)
        => Tagged ssc ([Address] -> m (DList TxHistoryEntry))
    getBlockHistory = (lift .) <$> getBlockHistory

    default getLocalHistory
        :: (MonadTrans t, MonadTxHistory m', t m' ~ m)
        => [Address] -> m (DList TxHistoryEntry)
    getLocalHistory = lift . getLocalHistory

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

type GenesisHistoryFetcher m = ToilT () (GenesisToil (DB.BlockDBRedirect (TxHistoryRedirect m)))

instance
    ( MonadRealDB m
    , MonadDBRead m
    , MonadThrow m
    , WithLogger m
    , MonadSlots m
    , MonadThrow m
    , MonadRealDB m
    , Ether.MonadReader' GenesisUtxo m
    , MonadTxpMem TxpExtra_TMP m
    , MonadBaseControl IO m
    , t ~ IdentityT
    ) => MonadTxHistory (Ether.TaggedTrans TxHistoryRedirectTag t m)
  where
    getBlockHistory
        :: forall ssc. SscHelpersClass ssc
        => Tagged ssc ([Address] -> TxHistoryRedirect m (DList TxHistoryEntry))
    getBlockHistory = Tagged $ \addrs -> do
        bot <- GS.getBot
        -- AJ: TODO: Efficiency
        sd <- HM.fromList <$> GS.getAllSlottingData

        let fromBlund :: Blund ssc -> GenesisHistoryFetcher m (Block ssc)
            fromBlund = pure . fst

            getBlockTimestamp :: MainBlock ssc -> Maybe Timestamp
            getBlockTimestamp blk = getSlotStartPure (blk ^. mainBlockSlot) sd

            blockFetcher :: HeaderHash -> GenesisHistoryFetcher m (DList TxHistoryEntry)
            blockFetcher start = GS.foldlUpWhileM fromBlund start (const $ const True)
                (deriveAddrHistoryBlk addrs getBlockTimestamp) mempty

        DB.runBlockDBRedirect . runGenesisToil . evalToilTEmpty $
            blockFetcher bot

    getLocalHistory :: [Address] -> TxHistoryRedirect m (DList TxHistoryEntry)
    getLocalHistory addrs = runDBTxp . evalToilTEmpty $ do
        let mapper (txid, TxAux {..}) =
                (WithHash taTx txid, taWitness, taDistribution)
            topsortErr = TxpInternalError
                "getLocalHistory: transactions couldn't be topsorted!"
        ltxs <- map mapper <$> getLocalTxs
        txs <- getRelatedTxsByAddrs addrs Nothing Nothing =<<
               maybeThrow topsortErr (topsortTxs (view _1) ltxs)
        return $ DL.fromList txs

#ifdef WITH_EXPLORER
    saveTx txw = () <$ runExceptT (eTxProcessTransaction txw)
#else
    saveTx txw = () <$ runExceptT (txProcessTransaction txw)
#endif
