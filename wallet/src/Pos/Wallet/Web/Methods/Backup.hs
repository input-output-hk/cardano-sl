{-# LANGUAGE TypeFamilies #-}

-- | Wallet web server.

module Pos.Wallet.Web.Methods.Backup
       ( importWalletJSON
       , exportWalletJSON
       ) where

import           Universum

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import           Formatting (sformat, stext, (%))

import           Pos.Core.NetworkMagic (NetworkMagic)
import           Pos.Util (HasLens (..))
import           Pos.Wallet.Web.Backup (TotalBackup (..), getWalletBackup)
import           Pos.Wallet.Web.ClientTypes (CFilePath (..), CId, CWallet, Wal)
import           Pos.Wallet.Web.Error (WalletError (..))
import qualified Pos.Wallet.Web.Methods.Logic as L
import           Pos.Wallet.Web.Methods.Restore (restoreWalletFromBackup)
import           Pos.Wallet.Web.State.State (askWalletSnapshot)
import           Pos.Wallet.Web.Tracking.Types (SyncQueue)
import           Servant.API.ContentTypes (NoContent (..))
import           UnliftIO (MonadUnliftIO)

type MonadWalletBackup ctx m = ( L.MonadWalletLogic ctx m
                               , MonadUnliftIO m
                               , HasLens SyncQueue ctx SyncQueue
                               )

importWalletJSON
    :: MonadWalletBackup ctx m
    => NetworkMagic
    -> CFilePath
    -> m CWallet
importWalletJSON nm (CFilePath (toString -> fp)) = do
    contents <- liftIO $ BSL.readFile fp
    TotalBackup wBackup <- either parseErr pure $ A.eitherDecode contents
    restoreWalletFromBackup nm wBackup
  where
    parseErr err = throwM . RequestError $
        sformat ("Error while reading JSON backup file: "%stext) $
        toText err

exportWalletJSON
    :: MonadWalletBackup ctx m
    => NetworkMagic
    -> CId Wal
    -> CFilePath
    -> m NoContent
exportWalletJSON nm wid (CFilePath (toString -> fp)) = do
    ws <- askWalletSnapshot
    wBackup <- TotalBackup <$> getWalletBackup nm ws wid
    liftIO $ BSL.writeFile fp $ A.encode wBackup
    return NoContent
