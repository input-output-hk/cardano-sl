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

import           Pos.Chain.Genesis as Genesis (Config (..))
import           Pos.Core.NetworkMagic (makeNetworkMagic)
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

importWalletJSON :: MonadWalletBackup ctx m => Genesis.Config -> CFilePath -> m CWallet
importWalletJSON genesisConfig (CFilePath (toString -> fp)) = do
    contents <- liftIO $ BSL.readFile fp
    TotalBackup wBackup <- either parseErr pure $ A.eitherDecode contents
    restoreWalletFromBackup genesisConfig wBackup
  where
    parseErr err = throwM . RequestError $
        sformat ("Error while reading JSON backup file: "%stext) $
        toText err

exportWalletJSON
    :: MonadWalletBackup ctx m
    => Genesis.Config
    -> CId Wal
    -> CFilePath
    -> m NoContent
exportWalletJSON genesisConfig wid (CFilePath (toString -> fp)) = do
    ws <- askWalletSnapshot
    let nm = makeNetworkMagic $ configProtocolMagic genesisConfig
    wBackup <- TotalBackup <$> getWalletBackup nm ws wid
    liftIO $ BSL.writeFile fp $ A.encode wBackup
    return NoContent
