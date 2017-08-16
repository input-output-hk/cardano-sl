{-# LANGUAGE TypeFamilies #-}

-- | Wallet web server.

module Pos.Wallet.Web.Methods.Backup
       ( importStateJSON
       , exportStateJSON
       ) where

import           Universum

import           Control.Lens                 (each)
import qualified Data.Aeson                   as A
import qualified Data.ByteString.Lazy         as BSL
import qualified Data.HashMap.Strict          as HM
import           Formatting                   (build, sformat, stext, (%))
import           System.Wlog                  (logWarning)

import           Pos.Aeson.ClientTypes        ()
import           Pos.Aeson.WalletBackup       ()
import           Pos.Wallet.KeyStorage        (addSecretKey)
import           Pos.Wallet.Web.Account       (GenSeed (..), genUniqueAccountId)
import           Pos.Wallet.Web.Backup        (AccountMetaBackup (..), StateBackup (..),
                                               WalletBackup (..), WalletMetaBackup (..),
                                               getStateBackup)
import           Pos.Wallet.Web.ClientTypes   (CWallet, encToCId)
import           Pos.Wallet.Web.Error         (WalletError (..))
import qualified Pos.Wallet.Web.Methods.Logic as L
import           Pos.Wallet.Web.Mode          (MonadWalletWebMode)
import           Pos.Wallet.Web.State         (createAccount, getWalletMeta)
import           Pos.Wallet.Web.Tracking      (syncWalletOnImport)

restoreWalletFromBackup :: MonadWalletWebMode m => WalletBackup -> m (Maybe CWallet)
restoreWalletFromBackup WalletBackup {..} = do
    let wId = encToCId wbSecretKey
    wExists <- isJust <$> getWalletMeta wId

    if wExists
        then do
            logWarning $
                sformat ("Wallet with id "%build%" already exists") wId
            pure Nothing
        else do
            let (WalletMetaBackup wMeta) = wbMeta
                accList = HM.toList wbAccounts
                          & each . _2 %~ \(AccountMetaBackup am) -> am

            addSecretKey wbSecretKey
            for_ accList $ \(idx, meta) -> do
                let aIdx = fromInteger $ fromIntegral idx
                    seedGen = DeterminedSeed aIdx
                accId <- genUniqueAccountId seedGen wId
                createAccount accId meta
            void $ L.createWalletSafe wId wMeta
            void $ syncWalletOnImport wbSecretKey
            -- Get wallet again to return correct balance and stuff
            Just <$> L.getWallet wId

restoreStateFromBackup :: MonadWalletWebMode m => StateBackup -> m [CWallet]
restoreStateFromBackup (FullStateBackup walletBackups) =
    catMaybes <$> forM walletBackups restoreWalletFromBackup

importStateJSON :: MonadWalletWebMode m => Text -> m [CWallet]
importStateJSON (toString -> fp) = do
    contents <- liftIO $ BSL.readFile fp
    wState <- either parseErr pure $ A.eitherDecode contents
    restoreStateFromBackup wState
  where
    parseErr err = throwM . RequestError $
        sformat ("Error while reading JSON backup file: "%stext) $
        toText err

exportStateJSON :: MonadWalletWebMode m => Text -> m ()
exportStateJSON (toString -> fp) = do
    wState <- getStateBackup
    liftIO $ BSL.writeFile fp $ A.encode wState

