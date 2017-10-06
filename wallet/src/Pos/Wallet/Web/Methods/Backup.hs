{-# LANGUAGE TypeFamilies #-}

-- | Wallet web server.

module Pos.Wallet.Web.Methods.Backup
       ( importWalletJSON
       , exportWalletJSON
       ) where

import           Universum

import           Control.Lens                 (each)
import qualified Data.Aeson                   as A
import qualified Data.ByteString.Lazy         as BSL
import qualified Data.HashMap.Strict          as HM
import           Formatting                   (sformat, stext, (%))

import           Pos.Aeson.ClientTypes        ()
import           Pos.Aeson.WalletBackup       ()
import           Pos.Wallet.KeyStorage        (addSecretKey)
import           Pos.Wallet.Web.Account       (GenSeed (..), genUniqueAccountId)
import           Pos.Wallet.Web.Backup        (AccountMetaBackup (..), TotalBackup (..),
                                               WalletBackup (..), WalletMetaBackup (..),
                                               getWalletBackup)
import           Pos.Wallet.Web.ClientTypes   (CId, CWallet, Wal, encToCId,
                                               CAccountMeta (..), CAccountInit (..))
import           Pos.Wallet.Web.Error         (WalletError (..))
import qualified Pos.Wallet.Web.Methods.Logic as L
import           Pos.Wallet.Web.Mode          (MonadWalletWebMode)
import           Pos.Wallet.Web.State         (createAccount, getWalletMeta)
import           Pos.Wallet.Web.Tracking      (syncWalletOnImport)

import           Pos.Crypto                   (firstHardened, emptyPassphrase)

restoreWalletFromBackup :: MonadWalletWebMode m => WalletBackup -> m CWallet
restoreWalletFromBackup WalletBackup {..} = do
    let wId = encToCId wbSecretKey
    wExists <- isJust <$> getWalletMeta wId

    if wExists
        then do
            throwM $ RequestError "Wallet with id already exists"

        else do
            let (WalletMetaBackup wMeta) = wbMeta
                accList = HM.toList wbAccounts
                          & each . _2 %~ \(AccountMetaBackup am) -> am

            addSecretKey wbSecretKey
            -- If there are no existing accounts, then create one
            if null accList
                then do
                    let idx = DeterminedSeed firstHardened
                        accMeta = CAccountMeta { caName = "Initial account" }
                        accInit = CAccountInit { caInitWId = wId, caInitMeta = accMeta }
                    () <$ L.newAccountIncludeUnready True idx emptyPassphrase accInit
                else for_ accList $ \(idx, meta) -> do
                    let aIdx = fromInteger $ fromIntegral idx
                        seedGen = DeterminedSeed aIdx
                    accId <- genUniqueAccountId seedGen wId
                    createAccount accId meta

            -- Restoring a wallet from backup may take a long time.
            -- Hence we mark the wallet as "not ready" until `syncWalletOnImport` completes.
            void $ L.createWalletSafe wId wMeta False
            -- `syncWalletOnImport` automatically marks a wallet as "ready".
            void $ syncWalletOnImport wbSecretKey
            -- Get wallet again to return correct balance and stuff
            L.getWallet wId

importWalletJSON :: MonadWalletWebMode m => Text -> m CWallet
importWalletJSON (toString -> fp) = do
    contents <- liftIO $ BSL.readFile fp
    TotalBackup wBackup <- either parseErr pure $ A.eitherDecode contents
    restoreWalletFromBackup wBackup
  where
    parseErr err = throwM . RequestError $
        sformat ("Error while reading JSON backup file: "%stext) $
        toText err

exportWalletJSON :: MonadWalletWebMode m => CId Wal -> Text -> m ()
exportWalletJSON wid (toString -> fp) = do
    wBackup <- TotalBackup <$> getWalletBackup wid
    liftIO $ BSL.writeFile fp $ A.encode wBackup
