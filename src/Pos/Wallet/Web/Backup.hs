module Pos.Wallet.Web.Backup
       ( WalletMetaBackup (..)
       , AccountMetaBackup (..)
       , WalletBackup (..)
       , StateBackup (..)
       , currentBackupFormatVersion
       , getWalletBackup
       , getStateBackup
       ) where

import           Universum

import qualified Data.HashMap.Strict        as HM

import           Pos.Crypto                 (EncryptedSecretKey)
import           Pos.Util.Util              (maybeThrow)
import           Pos.Wallet.Web.Account     (AccountMode, getSKByAddr)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), CAccountMeta (..), CId,
                                             CWalletMeta (..), Wal)
import           Pos.Wallet.Web.Error       (WalletError (..))
import           Pos.Wallet.Web.State       (getAccountMeta, getWalletAddresses,
                                             getWalletMeta)
import           Pos.Wallet.Web.Util        (getWalletAccountIds)

-- TODO: use `Data.Versions.SemVer` datatype for
-- accurate parsing and comparisons
currentBackupFormatVersion :: Text
currentBackupFormatVersion = "1.0.0"

newtype WalletMetaBackup = WalletMetaBackup CWalletMeta
newtype AccountMetaBackup = AccountMetaBackup CAccountMeta

data WalletBackup = WalletBackup
    { wbSecretKey :: !EncryptedSecretKey
    , wbMeta      :: !WalletMetaBackup
    , wbAccounts  :: !(HashMap Int AccountMetaBackup)
    }

data StateBackup = FullStateBackup [WalletBackup]

getWalletBackup :: AccountMode m => CId Wal -> m WalletBackup
getWalletBackup wId = do
    sk <- getSKByAddr wId
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

getStateBackup :: AccountMode m => m StateBackup
getStateBackup = getWalletAddresses >>= fmap FullStateBackup . mapM getWalletBackup
