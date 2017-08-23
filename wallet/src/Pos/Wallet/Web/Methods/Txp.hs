{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Utils for payments and redeeming.

module Pos.Wallet.Web.Methods.Txp
    ( rewrapTxError
    , coinDistrToOutputs
    , submitAndSaveReclaimableTx
    ) where

import           Universum

import           Control.Monad.Catch         (Handler (..), catches)
import qualified Data.List.NonEmpty          as NE
import           Formatting                  (build, sformat, shown, stext, (%))
import           Pos.Communication           (EnqueueMsg, TxMode, submitAndSaveTx)
import           System.Wlog                 (logInfo)

import           Pos.Client.Txp.Util         (TxError (..))
import           Pos.Core.Types              (Coin)
import           Pos.Txp                     (TxAux, TxOut (..), TxOutAux (..))
import           Pos.Wallet.Web.ClientTypes  (Addr, CId)
import           Pos.Wallet.Web.Error        (WalletError (..), rewrapToWalletError)
import           Pos.Wallet.Web.Pending.Util (isReclaimableFailure)
import           Pos.Wallet.Web.Util         (decodeCTypeOrFail)


rewrapTxError
    :: forall m a. MonadCatch m
    => Text -> m a -> m a
rewrapTxError prefix =
    rewrapToWalletError (\SomeException{} -> True) (InternalError . sbuild) .
    rewrapToWalletError (\TxError{} -> True) (RequestError . sbuild)
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
    mkTxOut addr coin = TxOutAux (TxOut addr coin) []

-- | Like 'submitAndSaveTx', but suppresses errors which can get gone
-- by the time of resubmission.
submitAndSaveReclaimableTx
    :: (TxMode ssc ctx m, MonadCatch m)
    => EnqueueMsg m -> TxAux -> m ()
submitAndSaveReclaimableTx enqueue txAux =
    void (submitAndSaveTx enqueue txAux)
        `catches`
        [ Handler $ \e ->
            if isReclaimableFailure e
                then ignore "reclaimable" e
                else throwM e

        , Handler $ \(SomeException e) ->
            -- I don't know where this error can came from,
            -- but it's better to try with tx again than to regret, right?
            -- At the moment of writting this all networking errors are suppresed
            ignore "unknown error" e
        ]
  where
    ignore desc e =
        logInfo $
        sformat ("Transaction creation failed ("%shown%" - "%stext%
                 "), but was given a second chance") e desc

