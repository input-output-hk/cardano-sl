module Cardano.Wallet.WalletLayer.Kernel.Transactions (
    getTransactions
) where

import           Universum

import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.InDb (InDb (..))
import           Cardano.Wallet.Kernel.DB.TxMeta (TxMeta (..))
import qualified Cardano.Wallet.Kernel.DB.TxMeta as TxMeta
import           Cardano.Wallet.WalletLayer.Types (GetTxError (..))

import           Cardano.Wallet.API.Indices
import           Cardano.Wallet.API.Request
import qualified Cardano.Wallet.API.Request.Filter as F
import           Cardano.Wallet.API.Request.Pagination
import qualified Cardano.Wallet.API.Request.Sort as S
import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.V1.Types (V1 (..), unV1)
import qualified Cardano.Wallet.API.V1.Types as V1
import           GHC.TypeLits (symbolVal)
import           Pos.Core as Core

getTransactions :: MonadIO m
                => Kernel.PassiveWallet
                -> Maybe V1.WalletId
                -> Maybe V1.AccountIndex
                -> Maybe (V1 Address)
                -> RequestParams
                -> FilterOperations V1.Transaction
                -> SortOperations V1.Transaction
                -> m (Either GetTxError (WalletResponse [V1.Transaction]))
getTransactions wallet mbWalletId mbAccountIndex mbAddress params fop sop = liftIO $ do
    let PaginationParams{..}  = rpPaginationParams params
    let PerPage pp = ppPerPage
    let Page cp = ppPage
    case castAccountFiltering mbWalletId mbAccountIndex of
        Left err          -> return $ Left err
        Right accountFops -> do
            case castSorting sop of
                Left err -> return $ Left err
                Right mbSorting -> do
                    db <- Kernel.getWalletSnapshot wallet
                    -- check Pos.Core.ProtocolConstants, Core.blkSecurityParam
                    let k = (2000 :: Word)           -- TODO: retrieve this constant from Core
                    let currentSlot = (1000 :: Word) -- TODO: retrieve this constant from Core ??
                    (meta, mbTotalEntries) <- TxMeta.getTxMetas
                        (wallet ^. Kernel.walletMeta)
                        (TxMeta.Offset . fromIntegral $ (cp - 1) * pp)
                        (TxMeta.Limit . fromIntegral $ pp)
                        accountFops
                        (unV1 <$> mbAddress)
                        (castFiltering $ mapIx unV1 <$> F.findMatchingFilterOp fop)
                        (castFiltering $ mapIx unV1 <$> F.findMatchingFilterOp fop)
                        mbSorting
                    let txs = map (metaToTx db k currentSlot) meta
                    return $ Right $ respond params txs mbTotalEntries


-- | Type Casting for Account filtering from V1 to MetaData Types.
castAccountFiltering :: Maybe V1.WalletId -> Maybe V1.AccountIndex -> Either GetTxError TxMeta.AccountFops
castAccountFiltering mbWalletId mbAccountIndex =
    case (mbWalletId, mbAccountIndex) of
        (Nothing, Nothing) -> Right TxMeta.Everything
        (Nothing, Just _)  -> Left GetTxMissingWalletIdError
        -- AccountIndex doesn`t uniquely identify an Account, so we shouldn`t continue without a WalletId.
        (Just (V1.WalletId wId), _) ->
            case decodeTextAddress wId of
                Left _         -> Left $ GetTxAddressDecodingFailed wId
                Right rootAddr -> Right $ TxMeta.AccountFops rootAddr mbAccountIndex

-- This function reads at most the head of the SortOperations and expects to find "created_at".
castSorting :: S.SortOperations V1.Transaction -> Either GetTxError (Maybe TxMeta.Sorting)
castSorting S.NoSorts = Right Nothing
castSorting (S.SortOp (sop :: S.SortOperation ix V1.Transaction) _) =
    case symbolVal (Proxy @(IndexToQueryParam V1.Transaction ix)) of
        "created_at" -> Right $ Just $ TxMeta.Sorting TxMeta.SortByCreationAt (castSortingDirection sop)
        txt -> Left $ GetTxInvalidSortingOperaration txt

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

metaToTx :: Kernel.DB -> Word -> Word -> TxMeta -> V1.Transaction
metaToTx db k current TxMeta{..} =
    V1.Transaction {
        txId = V1 _txMetaId,
        txConfirmations = confirmations,
        txAmount = V1 _txMetaAmount,
        txInputs = toPayDistr <$> _txMetaInputs,
        txOutputs = toPayDistr <$> _txMetaOutputs,
        txType = if _txMetaIsLocal then V1.LocalTransaction else V1.ForeignTransaction,
        txDirection = if _txMetaIsOutgoing then V1.OutgoingTransaction else V1.IncomingTransaction,
        txCreationTime = V1 _txMetaCreationAt,
        txStatus = status
    }

        where
            hdAccountId = HD.HdAccountId (HD.HdRootId $ InDb _txMetaWalletId)
                                        (HD.HdAccountIx _txMetaAccountId)

            toPayDistr :: (Address, Coin) -> V1.PaymentDistribution
            toPayDistr (addr, c) = V1.PaymentDistribution (V1 addr) (V1 c)

            mSlot = Kernel.accountTxSlot db hdAccountId _txMetaId
            isPending = Kernel.accountIsTxPending db hdAccountId _txMetaId

            (status, confirmations) = buildDynamicTxMeta mSlot k current isPending

buildDynamicTxMeta :: Maybe SlotId -> Word -> Word -> Bool -> (V1.TransactionStatus, Word)
buildDynamicTxMeta mSlot k currentSlot isPending = case isPending of
    True  -> (V1.Applying, 0)
    False ->
        case mSlot of
        Nothing     -> (V1.WontApply, 0)
        Just (SlotId (EpochIndex w64) (UnsafeLocalSlotIndex w16)) ->
            case ((fromIntegral currentSlot) - w64*(fromIntegral k) + (fromIntegral w16) >= fromIntegral k) of -- TODO: fix
            True  -> (V1.InNewestBlocks, fromIntegral w64) -- TODO: fix
            False -> (V1.Persisted, fromIntegral w16)      -- TODO: fix


-- | We don`t fitler in memory, so totalEntries is unknown, unless TxMeta Database counts them for us.
-- It is possible due to some error, to have length ls < Page.
respond :: RequestParams -> [a] -> Maybe Int -> (WalletResponse [a])
respond RequestParams{..} ls mbTotalEntries =
    let totalEntries = case mbTotalEntries of
            Nothing -> 0
            Just t  -> t
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
