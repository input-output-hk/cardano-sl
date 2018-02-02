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
import           Pos.Wallet.Web.ClientTypes   (CFilePath (..), CId, CWallet, Wal,
                                               encToCId)
import           Pos.Wallet.Web.Error         (WalletError (..))
import qualified Pos.Wallet.Web.Methods.Logic as L
import           Pos.Wallet.Web.Mode          (MonadWalletWebMode)
import           Pos.Wallet.Web.State         ( askWalletDB, getWalletSnapshot, createAccount
                                              , getWalletMeta, askWalletSnapshot)
import           Pos.Wallet.Web.Tracking      (syncWalletOnImport)

restoreWalletFromBackup :: MonadWalletWebMode m => WalletBackup -> m CWallet
restoreWalletFromBackup WalletBackup {..} = do
    let wId = encToCId wbSecretKey

    wExists <- isJust . flip getWalletMeta wId <$> askWalletSnapshot
    if wExists
        then do
            throwM $ RequestError "Wallet with id already exists"

        else do
            let (WalletMetaBackup wMeta) = wbMeta
                accList = HM.toList wbAccounts
                          & each . _2 %~ \(AccountMetaBackup am) -> am

            addSecretKey wbSecretKey
            -- XXX Transaction
            db <- askWalletDB
            for_ accList $ \(idx, meta) -> do
                ws <- getWalletSnapshot db
                let aIdx = fromInteger $ fromIntegral idx
                    seedGen = DeterminedSeed aIdx
                accId <- genUniqueAccountId ws seedGen wId
                createAccount db accId meta
            -- Restoring a wallet from backup may take a long time.
            -- Hence we mark the wallet as "not ready" until `syncWalletOnImport` completes.
            void $ L.createWalletSafe wId wMeta False
            -- `syncWalletOnImport` automatically marks a wallet as "ready".
            void $ syncWalletOnImport wbSecretKey
            -- Get wallet again to return correct balance and stuff
            L.getWallet wId

importWalletJSON :: MonadWalletWebMode m => CFilePath -> m CWallet
importWalletJSON (CFilePath (toString -> fp)) = do
    contents <- liftIO $ BSL.readFile fp
    TotalBackup wBackup <- either parseErr pure $ A.eitherDecode contents
    restoreWalletFromBackup wBackup
  where
    parseErr err = throwM . RequestError $
        sformat ("Error while reading JSON backup file: "%stext) $
        toText err

exportWalletJSON :: MonadWalletWebMode m => CId Wal -> CFilePath -> m ()
exportWalletJSON wid (CFilePath (toString -> fp)) = do
    ws <- askWalletSnapshot
    wBackup <- TotalBackup <$> getWalletBackup ws wid
    liftIO $ BSL.writeFile fp $ A.encode wBackup
