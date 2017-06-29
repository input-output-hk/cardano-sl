{-# LANGUAGE CPP                 #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
       , getRelatedTxsByAddrs
       , deriveAddrHistory
       , deriveAddrHistoryBlk
       , getTxHistoryDefault
       , saveTxDefault
       ) where

import           Universum

import           Control.Lens                (makeLenses)
import           Control.Monad.Trans         (MonadTrans)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.DList                  (DList)
import qualified Data.DList                  as DL
import           Data.Tagged                 (Tagged (..))
import qualified Ether
import           System.Wlog                 (WithLogger)

import           Pos.Block.Core              (Block, MainBlock, mainBlockSlot,
                                              mainBlockTxPayload)
import           Pos.Block.Types             (Blund)
import           Pos.Context                 (GenesisUtxo, genesisUtxoM)
import           Pos.Core                    (Address, ChainDifficulty, HeaderHash,
                                              Timestamp (..), difficultyL)
import           Pos.Crypto                  (WithHash (..), withHash)
import           Pos.DB                      (MonadDBRead, MonadGState, MonadRealDB)
import qualified Pos.DB.Block                as DB
import qualified Pos.DB.GState               as GS
import           Pos.Slotting                (MonadSlots, getSlotStartPure)
import           Pos.Ssc.Class               (SscHelpersClass)
#ifdef WITH_EXPLORER
import           Pos.Explorer.Txp.Local      (eTxProcessTransaction)
#else
import           Pos.Txp                     (txProcessTransaction)
#endif
import           Pos.Txp                     (MonadTxpMem, MonadUtxo, MonadUtxoRead,
                                              Tx (..), TxAux (..), TxDistribution, TxId,
                                              TxOut, TxOutAux (..), TxWitness, Utxo,
                                              UtxoStateT, applyTxToUtxo, evalUtxoStateT,
                                              flattenTxPayload, getLocalTxs,
                                              runUtxoStateT, txOutAddress, utxoGet)
import           Pos.WorkMode.Class          (TxpExtra_TMP)

-- Remove this once there's no #ifdef-ed Pos.Txp import
{-# ANN module ("HLint: ignore Use fewer imports" :: Text) #-}

data TxHistoryAnswer = TxHistoryAnswer
    { taLastCachedHash :: HeaderHash
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
    , _thTimestamp   :: !(Maybe Timestamp)
    } deriving (Show, Eq, Generic)

makeLenses ''TxHistoryEntry

-- | Select transactions by predicate on related addresses
getTxsByPredicate
    :: MonadUtxo m
    => ([Address] -> Bool)
    -> [(WithHash Tx, TxWitness, TxDistribution)]
    -> m [TxHistoryEntry]
getTxsByPredicate pr txs = go txs []
  where
    go [] acc = return acc
    go ((wh@(WithHash tx txId), _wit, dist) : rest) acc = do
        inputs <- getSenders tx
        let outgoings = toList $ txOutAddress <$> _txOutputs tx
        let incomings = ordNub $ map txOutAddress inputs

        applyTxToUtxo wh dist

        let acc' = if pr (incomings ++ outgoings)
                   then (THEntry txId tx inputs Nothing incomings outgoings Nothing : acc)
                   else acc
        go rest acc'

-- | Select transactions related to one of given addresses
getRelatedTxsByAddrs
    :: MonadUtxo m
    => [Address]
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
    txs <- getRelatedTxsByAddrs addrs . map mapper . flattenTxPayload $
           blk ^. mainBlockTxPayload
    let difficulty = blk ^. difficultyL
        alterEntry e = e & thDifficulty .~ Just difficulty
                         & thTimestamp .~ getTs blk
        txs' = map alterEntry txs
    return $ DL.fromList txs' <> hist

----------------------------------------------------------------------------
-- MonadTxHistory
----------------------------------------------------------------------------

-- | A class which have methods to get transaction history
class Monad m => MonadTxHistory m where
    getTxHistory
        :: SscHelpersClass ssc
        => Tagged ssc ([Address] -> Maybe (HeaderHash, Utxo) -> m TxHistoryAnswer)
    saveTx :: (TxId, TxAux) -> m ()

instance {-# OVERLAPPABLE #-}
    (MonadTxHistory m, MonadTrans t, Monad (t m)) =>
        MonadTxHistory (t m)
  where
    getTxHistory = fmap lift <<$>> getTxHistory
    saveTx = lift . saveTx

type TxHistoryEnv m =
    ( MonadBaseControl IO m
    , MonadDBRead m
    , MonadGState m
    , MonadTxpMem TxpExtra_TMP m
    , WithLogger m
    , MonadSlots m
    , MonadThrow m
    , MonadRealDB m
    , Ether.MonadReader' GenesisUtxo m
    )

getTxHistoryDefault
    :: forall ssc m. (SscHelpersClass ssc, TxHistoryEnv m)
    => Tagged ssc ([Address] -> Maybe (HeaderHash, Utxo) -> m TxHistoryAnswer)
getTxHistoryDefault = Tagged $ \addrs mInit -> do
    let getGenPair = (,) <$> GS.getBot <*> genesisUtxoM
    (bot, bottomUtxo) <- maybe getGenPair pure mInit
    sd <- GS.getSlottingData

    let fromBlund :: Blund ssc -> UtxoStateT (DB.BlockDBRedirect m) (Block ssc)
        fromBlund = pure . fst

        getBlockTimestamp :: MainBlock ssc -> Maybe Timestamp
        getBlockTimestamp blk = getSlotStartPure True (blk ^. mainBlockSlot) sd

        blockFetcher :: HeaderHash -> UtxoStateT (DB.BlockDBRedirect m) (DList TxHistoryEntry)
        blockFetcher start = GS.foldlUpWhileM fromBlund start (const $ const True)
            (deriveAddrHistoryBlk addrs getBlockTimestamp) mempty

        localFetcher :: UtxoStateT (DB.BlockDBRedirect m) (DList TxHistoryEntry)
        localFetcher = do
            let mapper (txid, TxAux {..}) =
                    (WithHash taTx txid, taWitness, taDistribution)
            ltxs <- lift . lift $ getLocalTxs
            txs <- getRelatedTxsByAddrs addrs $ map mapper ltxs
            return $ DL.fromList txs

    DB.runBlockDBRedirect $ do
        (blockTxs, cachedUtxo) <- runUtxoStateT (blockFetcher bot) bottomUtxo
        localTxs <- evalUtxoStateT localFetcher cachedUtxo
        tip <- GS.getTip
        return . TxHistoryAnswer tip cachedUtxo . DL.toList $ localTxs <> blockTxs

saveTxDefault :: TxHistoryEnv m => (TxId, TxAux) -> m ()
#ifdef WITH_EXPLORER
saveTxDefault txw = () <$ runExceptT (eTxProcessTransaction txw)
#else
saveTxDefault txw = () <$ runExceptT (txProcessTransaction txw)
#endif
