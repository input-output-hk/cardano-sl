{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Different utils for wallets

module Pos.Wallet.Web.Util
    ( getWalletAccountIds
    , rewrapTxError
    , decodeCTypeOrFail
    ) where

import           Universum

import           Formatting                 (build, sformat, stext, (%))

import           Pos.Client.Txp.Util        (TxError (..))
import           Pos.Util.Servant           (FromCType (..), OriginType)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), CId, Wal)
import           Pos.Wallet.Web.Error       (WalletError (..), rewrapToWalletError)
import           Pos.Wallet.Web.State       (WebWalletModeDB, getAccountIds)

getWalletAccountIds :: WebWalletModeDB ctx m => CId Wal -> m [AccountId]
getWalletAccountIds cWalId = filter ((== cWalId) . aiWId) <$> getAccountIds

rewrapTxError
    :: forall m a. MonadCatch m
    => Text -> m a -> m a
rewrapTxError prefix =
    rewrapToWalletError (const True) (InternalError . sbuild) .
    rewrapToWalletError (\TxError{} -> True) (RequestError . sbuild)
  where
    sbuild = sformat (stext%": "%build) prefix

decodeCTypeOrFail :: (MonadThrow m, FromCType c) => c -> m (OriginType c)
decodeCTypeOrFail = either wrongAddress pure . decodeCType
  where wrongAddress err = throwM . DecodeError $
            sformat ("Error while decoding CId: "%stext) err
