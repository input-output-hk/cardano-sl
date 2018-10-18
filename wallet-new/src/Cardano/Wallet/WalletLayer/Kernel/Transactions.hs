module Cardano.Wallet.WalletLayer.Kernel.Transactions (
      getTransactions
    , toTransaction
) where

import           Universum

import           Control.Monad.Except
import           GHC.TypeLits (symbolVal)

import           Pos.Chain.Txp (TxId)
import           Pos.Core (Address, Coin, SlotCount, SlotId, Timestamp,
                     decodeTextAddress, flattenSlotId, getBlockCount)

import           Cardano.Wallet.API.Indices
import           Cardano.Wallet.API.Request
import qualified Cardano.Wallet.API.Request.Filter as F
import           Cardano.Wallet.API.Request.Pagination
import qualified Cardano.Wallet.API.Request.Sort as S
import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.V1.Types (V1 (..), unV1)
import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.InDb (InDb (..))
import           Cardano.Wallet.Kernel.DB.TxMeta (TxMeta (..))
import qualified Cardano.Wallet.Kernel.DB.TxMeta as TxMeta
import qualified Cardano.Wallet.Kernel.Internal as Kernel
import qualified Cardano.Wallet.Kernel.NodeStateAdaptor as Node
import qualified Cardano.Wallet.Kernel.Read as Kernel
import           Cardano.Wallet.WalletLayer (GetTxError (..))
import           UTxO.Util (exceptT)

getTransactions :: MonadIO m
                => Kernel.PassiveWallet
                -> Maybe V1.WalletId
                -> Maybe V1.AccountIndex
                -> Maybe (V1 Address)
                -> RequestParams
                -> FilterOperations '[V1 TxId, V1 Timestamp] V1.Transaction
                -> SortOperations V1.Transaction
                -> m (Either GetTxError (WalletResponse [V1.Transaction]))
getTransactions wallet mbWalletId mbAccountIndex mbAddress params fop sop = liftIO $ runExceptT $ do
    let PaginationParams{..}  = rpPaginationParams params
    let PerPage pp = ppPerPage
    let Page cp = ppPage
    accountFops <- castAccountFiltering mbWalletId mbAccountIndex
    mbSorting <- castSorting sop
    db <- liftIO $ Kernel.getWalletSnapshot wallet
    sc <- liftIO $ Node.getSlotCount (wallet ^. Kernel.walletNode)
    currentSlot <- liftIO $ Node.getTipSlotId (wallet ^. Kernel.walletNode)
    -- things we read from sqlite, may fail to be transformed to Txs. So we
    -- keep querying in a loop.
    -- loop invariants:
    --    + length accTxs == accTxsLength
    --    + accTxsLength < pp
    --    + accTrials <= trialsThreshold
    --    + localLimit <= localLimitThreshold
    let go :: Int -> Int -> Int -> ([V1.Transaction], Int) -> Maybe Int -> IO (Either GetTxError (WalletResponse [V1.Transaction]))
        go accTrials localOffset localLimit (accTxs, accTxsLength) accTotalEntries = do
            (newMetas, newTotalEntries) <- TxMeta.getTxMetas
                (wallet ^. Kernel.walletMeta)
                (TxMeta.Offset . fromIntegral $ (cp - 1) * pp + localOffset)
                (TxMeta.Limit . fromIntegral $ localLimit)
                accountFops
                (unV1 <$> mbAddress)
                (castFiltering $ mapIx unV1 <$> F.findMatchingFilterOp fop)
                (castFiltering $ mapIx unV1 <$> F.findMatchingFilterOp fop)
                mbSorting
                (accTrials == 0) -- we only count total results in first loop.
            let newMetasLength = length newMetas
            -- txs which don't have account in acid-state are filtered out.
            newTxs <- catMaybes <$> mapM (\m -> rightToMaybe <$> (runExceptT $ metaToTx db sc currentSlot m)) newMetas
            -- txs which are invalid are filtered out.
            let newValidTxs = filter isValid newTxs
            let newValidTxsLength = length newValidTxs

            let txs = accTxs <> newValidTxs
            let txsLength = accTxsLength + length newValidTxs
            let mbTotalEntries = if accTrials == 0 then newTotalEntries else accTotalEntries
            let newLocalOffset = localOffset + localLimit
            let trials = accTrials + 1
            if trials > trialsThreshold || txsLength >= pp || localLimit > localLimitThreshold
               || newMetasLength < localLimit
                then return $ Right $ respond params (take pp txs) mbTotalEntries
                else do
                    let newLocalLimit = if newValidTxsLength == 0
                            then 2 * localLimit
                            else localLimit
                    go trials newLocalOffset newLocalLimit (txs, txsLength) mbTotalEntries
    ExceptT $ liftIO $ go 0 0 pp ([], 0) Nothing

trialsThreshold :: Int
trialsThreshold = 7

localLimitThreshold :: Int
localLimitThreshold = 1000

isValid :: V1.Transaction -> Bool
isValid tx = not (V1.txDirection tx == V1.IncomingTransaction
                 && ((V1.txStatus tx == V1.Applying) || (V1.txStatus tx == V1.WontApply)))

toTransaction :: MonadIO m
              => Kernel.PassiveWallet
              -> TxMeta
              -> m (Either HD.UnknownHdAccount V1.Transaction)
toTransaction wallet meta = liftIO $ do
    db <- liftIO $ Kernel.getWalletSnapshot wallet
    sc <- liftIO $ Node.getSlotCount (wallet ^. Kernel.walletNode)
    currentSlot <- Node.getTipSlotId (wallet ^. Kernel.walletNode)
    return $ runExcept $ metaToTx db sc currentSlot meta

-- | Type Casting for Account filtering from V1 to MetaData Types.
castAccountFiltering :: Monad m => Maybe V1.WalletId -> Maybe V1.AccountIndex -> ExceptT GetTxError m TxMeta.AccountFops
castAccountFiltering mbWalletId mbAccountIndex =
    case (mbWalletId, mbAccountIndex) of
        (Nothing, Nothing) -> return TxMeta.Everything
        (Nothing, Just _)  -> throwError GetTxMissingWalletIdError
        -- AccountIndex doesn`t uniquely identify an Account, so we shouldn`t continue without a WalletId.
        (Just (V1.WalletId wId), _) ->
            case decodeTextAddress wId of
                Left _         -> throwError $ GetTxAddressDecodingFailed wId
                Right rootAddr -> return $ TxMeta.AccountFops rootAddr (V1.getAccIndex <$> mbAccountIndex)

-- This function reads at most the head of the SortOperations and expects to find "created_at".
castSorting :: Monad m => S.SortOperations V1.Transaction -> ExceptT GetTxError m (Maybe TxMeta.Sorting)
castSorting S.NoSorts = return Nothing
castSorting (S.SortOp (sop :: S.SortOperation ix V1.Transaction) _) =
    case symbolVal (Proxy @(IndexToQueryParam V1.Transaction ix)) of
        "created_at" -> return $ Just $ TxMeta.Sorting TxMeta.SortByCreationAt (castSortingDirection sop)
        txt -> throwError $ GetTxInvalidSortingOperation txt

castSortingDirection :: S.SortOperation ix a -> TxMeta.SortDirection
castSortingDirection (S.SortByIndex srt _) = case srt of
    S.SortAscending  -> TxMeta.Ascending
    S.SortDescending -> TxMeta.Descending

castFiltering :: Maybe (F.FilterOperation ix V1.Transaction) -> TxMeta.FilterOperation ix
castFiltering mfop = case mfop of
    Nothing -> TxMeta.NoFilterOp
    Just fop -> case fop of
        (F.FilterByIndex q)         -> TxMeta.FilterByIndex q
        (F.FilterByPredicate prd q) -> TxMeta.FilterByPredicate (castFilterOrd prd) q
        (F.FilterByRange q w)       -> TxMeta.FilterByRange q w
        (F.FilterIn ls)             -> TxMeta.FilterIn ls

castFilterOrd :: F.FilterOrdering -> TxMeta.FilterOrdering
castFilterOrd pr = case pr of
    F.Equal            -> TxMeta.Equal
    F.GreaterThan      -> TxMeta.GreaterThan
    F.GreaterThanEqual -> TxMeta.GreaterThanEqual
    F.LesserThan       -> TxMeta.LesserThan
    F.LesserThanEqual  -> TxMeta.LesserThanEqual

metaToTx :: Monad m => Kernel.DB -> SlotCount -> SlotId -> TxMeta -> ExceptT HD.UnknownHdAccount m V1.Transaction
metaToTx db slotCount current TxMeta{..} = do
    mSlotwithState <- withExceptT identity $ exceptT $
                        Kernel.currentTxSlotId db _txMetaId hdAccountId
    isPending      <- withExceptT identity $ exceptT $
                        Kernel.currentTxIsPending db _txMetaId hdAccountId
    assuranceLevel <- withExceptT HD.embedUnknownHdRoot $ exceptT $
                        Kernel.rootAssuranceLevel db hdRootId
    let (status, confirmations) = buildDynamicTxMeta assuranceLevel slotCount mSlotwithState current isPending
    return V1.Transaction {
        txId = V1 _txMetaId,
        txConfirmations = fromIntegral confirmations,
        txAmount = V1 _txMetaAmount,
        txInputs = inputsToPayDistr <$> _txMetaInputs,
        txOutputs = outputsToPayDistr <$> _txMetaOutputs,
        txType = if _txMetaIsLocal then V1.LocalTransaction else V1.ForeignTransaction,
        txDirection = if _txMetaIsOutgoing then V1.OutgoingTransaction else V1.IncomingTransaction,
        txCreationTime = V1 _txMetaCreationAt,
        txStatus = status
    }
        where
            hdRootId    = HD.HdRootId $ InDb _txMetaWalletId
            hdAccountId = HD.HdAccountId hdRootId (HD.HdAccountIx _txMetaAccountIx)

            inputsToPayDistr :: (a , b, Address, Coin) -> V1.PaymentDistribution
            inputsToPayDistr (_, _, addr, c) = V1.PaymentDistribution (V1 addr) (V1 c)

            outputsToPayDistr :: (Address, Coin) -> V1.PaymentDistribution
            outputsToPayDistr (addr, c) = V1.PaymentDistribution (V1 addr) (V1 c)

buildDynamicTxMeta :: HD.AssuranceLevel -> SlotCount -> HD.CombinedWithAccountState (Maybe SlotId) -> SlotId -> Bool -> (V1.TransactionStatus, Word64)
buildDynamicTxMeta assuranceLevel slotCount mSlotwithState currentSlot isPending =
    let currentSlot' = flattenSlotId slotCount currentSlot
        goWithSlot confirmedIn =
            let confirmedIn'  = flattenSlotId slotCount confirmedIn
                confirmations = currentSlot' - confirmedIn'
            in case (confirmations < getBlockCount (HD.assuredBlockDepth assuranceLevel)) of
                True  -> (V1.InNewestBlocks, confirmations)
                False -> (V1.Persisted, confirmations)
    in case isPending of
        True  -> (V1.Applying, 0)
        False ->
            case mSlotwithState of
                HD.UpToDate Nothing     -> (V1.WontApply, 0)
                HD.Incomplete Nothing   -> (V1.Applying, 0)  -- during restoration, we report txs not found yet as Applying.
                HD.UpToDate (Just sl)   -> goWithSlot sl
                HD.Incomplete (Just sl) -> goWithSlot sl

-- | We don`t fitler in memory, so totalEntries is unknown, unless TxMeta Database counts them for us.
-- It is possible due to some error, to have length ls < Page.
-- This can happen when a Tx is found without Inputs.
respond :: RequestParams -> [a] -> Maybe Int -> (WalletResponse [a])
respond RequestParams{..} ls mbTotalEntries =
    let totalEntries = fromMaybe 0 mbTotalEntries
        PaginationParams{..}  = rpPaginationParams
        perPage@(PerPage pp)  = ppPerPage
        currentPage           = ppPage
        totalPages            = max 1 $ ceiling (fromIntegral totalEntries / (fromIntegral pp :: Double))
        metadata              = PaginationMetadata {
                                metaTotalPages = totalPages
                                , metaPage = currentPage
                                , metaPerPage = perPage
                                , metaTotalEntries = totalEntries
                                }
    in  WalletResponse {
        wrData = ls
      , wrStatus = SuccessStatus
      , wrMeta = Metadata metadata
      }
