{-# LANGUAGE TypeFamilies #-}

-- | Wallet web server.

module Pos.Wallet.Web.Methods.Backup
       ( importWalletJSON
       , exportWalletJSON
       , restoreWalletFromBackup
       -- ^ just for testing
       ) where

import           Universum

import           Control.Lens (each)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import           Formatting (sformat, stext, (%))

import           Pos.Client.KeyStorage (addSecretKey)
import           Pos.Wallet.Web.Account (GenSeed (..), genUniqueAccountId)
import           Pos.Wallet.Web.Backup (AccountMetaBackup (..), TotalBackup (..), WalletBackup (..),
                                        WalletMetaBackup (..), getWalletBackup)
import           Pos.Wallet.Web.ClientTypes (CAccountInit (..), CAccountMeta (..), CFilePath (..),
                                             CId, CWallet, Wal, encToCId)
import           Pos.Wallet.Web.Error (WalletError (..))
import qualified Pos.Wallet.Web.Methods.Logic as L
import           Pos.Wallet.Web.State (AddressLookupMode (Ever), createAccount,
                                       getAccountWAddresses, getWalletMeta)
import           Pos.Wallet.Web.Tracking (syncWalletOnImport)
import           Pos.Wallet.Web.Util (getWalletAccountIds)
import           Servant.API.ContentTypes (NoContent (..))

import           Pos.Crypto (emptyPassphrase, firstHardened)


type MonadWalletBackup ctx m = L.MonadWalletLogic ctx m

restoreWalletFromBackup :: MonadWalletBackup ctx m => WalletBackup -> m CWallet
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
                defaultAccAddrIdx = DeterminedSeed firstHardened

            addSecretKey wbSecretKey
            -- If there are no existing accounts, then create one
            if null accList
                then do
                    let accMeta = CAccountMeta { caName = "Initial account" }
                        accInit = CAccountInit { caInitWId = wId, caInitMeta = accMeta }
                    () <$ L.newAccountIncludeUnready True defaultAccAddrIdx emptyPassphrase accInit
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

            -- Get wallet accounts and create default address for each account
            -- without any existing address
            wAccIds <- getWalletAccountIds wId
            for_ wAccIds $ \accId -> getAccountWAddresses Ever accId >>= \case
                Nothing -> throwM $ InternalError "restoreWalletFromBackup: fatal: cannot find \
                                                  \an existing account of newly imported wallet"
                Just [] -> void $ L.newAddress defaultAccAddrIdx emptyPassphrase accId
                Just _  -> pure ()

            -- Get wallet again to return correct balance and stuff
            L.getWallet wId

importWalletJSON :: MonadWalletBackup ctx m => CFilePath -> m CWallet
importWalletJSON (CFilePath (toString -> fp)) = do
    contents <- liftIO $ BSL.readFile fp
    TotalBackup wBackup <- either parseErr pure $ A.eitherDecode contents
    restoreWalletFromBackup wBackup
  where
    parseErr err = throwM . RequestError $
        sformat ("Error while reading JSON backup file: "%stext) $
        toText err

exportWalletJSON :: MonadWalletBackup ctx m => CId Wal -> CFilePath -> m NoContent
exportWalletJSON wid (CFilePath (toString -> fp)) = do
    wBackup <- TotalBackup <$> getWalletBackup wid
    liftIO $ BSL.writeFile fp $ A.encode wBackup
    return NoContent
