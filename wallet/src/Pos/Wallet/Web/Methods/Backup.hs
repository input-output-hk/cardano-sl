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
import           Data.Default                 (def)
import qualified Data.HashMap.Strict          as HM
import           Formatting                   (build, sformat, stext, (%))

import           Pos.Aeson.ClientTypes        ()
import           Pos.Aeson.WalletBackup       ()
import           Pos.Crypto                   (PassPhrase, changeEncPassphrase,
                                               noPassEncrypt)
import           Pos.Util.Util                (maybeThrow)
import           Pos.Wallet.KeyStorage        (addSecretKey)
import           Pos.Wallet.Web.Account       (GenSeed (..), genUniqueAccountId)
import           Pos.Wallet.Web.Backup        (AccountMetaBackup (..), TotalBackup (..),
                                               WalletBackup (..), WalletMetaBackup (..),
                                               getWalletBackup)
import           Pos.Wallet.Web.ClientTypes   (CId, CWallet, Wal, encToCId)
import           Pos.Wallet.Web.Error         (WalletError (..))
import qualified Pos.Wallet.Web.Methods.Logic as L
import           Pos.Wallet.Web.Mode          (MonadWalletWebMode)
import           Pos.Wallet.Web.State         (createAccount, getWalletMeta)
import           Pos.Wallet.Web.Tracking      (syncWalletOnImport)

restoreWalletFromBackup
    :: MonadWalletWebMode m
    => PassPhrase -> WalletBackup -> m CWallet
restoreWalletFromBackup newPass WalletBackup {..} = do
    sk <- maybeThrow (InternalError "Bad secret key in backup") $
          changeEncPassphrase def newPass (noPassEncrypt wbSecretKey)
    let wId = encToCId sk
    wExists <- isJust <$> getWalletMeta wId

    if wExists
        then do
            throwM . RequestError $
                sformat ("Wallet with id "%build%" already exists") wId

        else do
            let (WalletMetaBackup wMeta) = wbMeta
                accList = HM.toList wbAccounts
                          & each . _2 %~ \(AccountMetaBackup am) -> am

            addSecretKey sk
            for_ accList $ \(idx, meta) -> do
                let aIdx = fromInteger $ fromIntegral idx
                    seedGen = DeterminedSeed aIdx
                accId <- genUniqueAccountId seedGen wId
                createAccount accId meta
            void $ L.createWalletSafe wId wMeta
            void $ syncWalletOnImport sk
            -- Get wallet again to return correct balance and stuff
            L.getWallet wId

importWalletJSON :: MonadWalletWebMode m => PassPhrase -> Text -> m CWallet
importWalletJSON passphrase (toString -> fp) = do
    contents <- liftIO $ BSL.readFile fp
    TotalBackup wBackup <- either parseErr pure $ A.eitherDecode contents
    restoreWalletFromBackup passphrase wBackup
  where
    parseErr err = throwM . RequestError $
        sformat ("Error while reading JSON backup file: "%stext) $
        toText err

exportWalletJSON :: MonadWalletWebMode m => PassPhrase -> CId Wal -> Text -> m ()
exportWalletJSON passphrase wid (toString -> fp) = do
    wBackup <- TotalBackup <$> getWalletBackup passphrase wid
    liftIO $ BSL.writeFile fp $ A.encode wBackup

