{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Utils for payments and redeeming.

module Pos.Wallet.Web.Methods.Txp
    ( MonadWalletTxFull
    , gatherPendingTxsSummary
    , rewrapTxError
    , coinDistrToOutputs
    , submitAndSaveNewPtx
    , getPendingAddresses
    ) where

import           Universum

import qualified Data.List.NonEmpty as NE
import           Formatting (build, sformat, stext, (%))

import           Pos.Client.KeyStorage (MonadKeys)
import           Pos.Client.Txp.Addresses (MonadAddresses (..))
import           Pos.Client.Txp.Util (InputSelectionPolicy (..), PendingAddresses (..),
                                      isCheckedTxError)
import           Pos.Core.Common (Coin)
import           Pos.Core.Txp (Tx (..), TxAux (..), TxOut (..), TxOutAux (..))
import           Pos.Crypto (PassPhrase, hash)
import           Pos.Util.Chrono (getNewestFirst, toNewestFirst)
import           Pos.Util.Servant (encodeCType)
import           Pos.Wallet.Web.ClientTypes (AccountId, Addr, CId)
import           Pos.Wallet.Web.Error (WalletError (..), rewrapToWalletError)
import           Pos.Wallet.Web.Methods.History (MonadWalletHistory)
import           Pos.Wallet.Web.Methods.Misc (PendingTxsSummary (..))
import           Pos.Wallet.Web.Mode (MonadWalletWebMode)
import           Pos.Wallet.Web.Pending (PendingTx (..), TxSubmissionMode, allPendingAddresses,
                                         isPtxInBlocks, ptxFirstSubmissionHandler, sortPtxsChrono)
import           Pos.Wallet.Web.Pending.Submission (submitAndSavePtx)
import           Pos.Wallet.Web.State (WalletDB, WalletSnapshot, askWalletSnapshot, getPendingTxs)
import           Pos.Wallet.Web.Util (decodeCTypeOrFail)


type MonadWalletTxFull ctx m =
    ( TxSubmissionMode ctx m
    , MonadWalletHistory ctx m
    , MonadKeys m
    , AddrData m ~ (AccountId, PassPhrase)
    )

rewrapTxError
    :: forall m a. MonadCatch m
    => Text -> m a -> m a
rewrapTxError prefix =
    rewrapToWalletError (\SomeException{} -> True) (InternalError . sbuild) .
    rewrapToWalletError isCheckedTxError (RequestError . sbuild)
  where
    sbuild :: Buildable e => e -> Text
    sbuild = sformat (stext%": "%build) prefix

coinDistrToOutputs
    :: MonadThrow m
    => NonEmpty (CId Addr, Coin)
    -> m (NonEmpty TxOutAux)
coinDistrToOutputs distr = do
    addrs <- mapM decodeCTypeOrFail cAddrs
    pure $ NE.zipWith mkTxOut addrs coins
  where
    (cAddrs, coins) = NE.unzip distr
    mkTxOut addr coin = TxOutAux (TxOut addr coin)

-- | Like 'submitAndSaveTx', but suppresses errors which can get gone
-- by the time of resubmission.
submitAndSaveNewPtx
    :: TxSubmissionMode ctx m
    => WalletDB
    -> (TxAux -> m Bool)
    -> PendingTx
    -> m ()
submitAndSaveNewPtx db submit = submitAndSavePtx db submit ptxFirstSubmissionHandler

gatherPendingTxsSummary :: MonadWalletWebMode ctx m => m [PendingTxsSummary]
gatherPendingTxsSummary =
    map mkInfo .
    getNewestFirst . toNewestFirst . sortPtxsChrono .
    filter unconfirmedPtx .
    getPendingTxs <$> askWalletSnapshot
  where
    unconfirmedPtx = not . isPtxInBlocks . _ptxCond
    mkInfo PendingTx{..} =
        let tx = taTx _ptxTxAux
        in  PendingTxsSummary
            { ptiSlot = _ptxCreationSlot
            , ptiCond = encodeCType (Just _ptxCond)
            , ptiInputs = _txInputs tx
            , ptiOutputs = _txOutputs tx
            , ptiTxId = hash tx
            }

-- | With regard to tx creation policy which is going to be used,
-- get addresses which are refered by some yet unconfirmed transaction outputs.
getPendingAddresses :: WalletSnapshot -> InputSelectionPolicy -> PendingAddresses
getPendingAddresses ws = \case
    OptimizeForSecurity ->
        -- NOTE (int-index) The pending transactions are ignored when we optimize
        -- for security, so it is faster to not get them. In case they start being
        -- used for other purposes, this shortcut must be removed.
        mempty
    OptimizeForHighThroughput ->
        allPendingAddresses (getPendingTxs ws)
