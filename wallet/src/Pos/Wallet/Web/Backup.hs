module Pos.Wallet.Web.Backup
       ( WalletMetaBackup (..)
       , AccountMetaBackup (..)
       , WalletBackup (..)
       , TotalBackup (..)
       , currentBackupFormatVersion
       , getWalletBackup
       ) where

import           Universum

import qualified Data.HashMap.Strict        as HM
import qualified Data.SemVer                as V

import           Pos.Crypto                 (PassPhrase, SecretKey, removeEncPassphrase)
import           Pos.Util.Util              (maybeThrow)
import           Pos.Wallet.Web.Account     (AccountMode, getSKById)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), CAccountMeta (..), CId,
                                             CWalletMeta (..), Wal)
import           Pos.Wallet.Web.Error       (WalletError (..))
import           Pos.Wallet.Web.State       (getAccountMeta, getWalletMeta)
import           Pos.Wallet.Web.Util        (getWalletAccountIds)

currentBackupFormatVersion :: V.Version
currentBackupFormatVersion = V.initial & V.major .~ 1

newtype WalletMetaBackup = WalletMetaBackup CWalletMeta
newtype AccountMetaBackup = AccountMetaBackup CAccountMeta

data WalletBackup = WalletBackup
    { wbSecretKey :: !SecretKey
    , wbMeta      :: !WalletMetaBackup
    , wbAccounts  :: !(HashMap Int AccountMetaBackup)
    }

data TotalBackup = TotalBackup WalletBackup

getWalletBackup :: AccountMode ctx m => PassPhrase -> CId Wal -> m WalletBackup
getWalletBackup passphrase wId = do
    encSK <- getSKById wId
    let msk = removeEncPassphrase passphrase encSK
    sk <- maybeThrow (RequestError "Passphrase doesn't match") msk

    meta <- maybeThrow (InternalError "Wallet have no meta") =<<
            getWalletMeta wId
    accountIds <- getWalletAccountIds wId
    accountMetas <- forM accountIds $
        maybeThrow (InternalError "Account have no meta") <=<
        getAccountMeta

    let accountsMap = HM.fromList $ zip
            (map (fromInteger . fromIntegral . aiIndex) accountIds)
            (map AccountMetaBackup accountMetas)

    return WalletBackup
        { wbSecretKey = sk
        , wbMeta = WalletMetaBackup meta
        , wbAccounts = accountsMap
        }
