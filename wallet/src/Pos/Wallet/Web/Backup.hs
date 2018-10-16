module Pos.Wallet.Web.Backup
       ( WalletMetaBackup (..)
       , AccountMetaBackup (..)
       , WalletBackup (..)
       , TotalBackup (..)
       , currentBackupFormatVersion
       , getWalletBackup
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import qualified Data.SemVer as V
import           Test.QuickCheck (Arbitrary (..), elements)

import           Pos.Core.NetworkMagic (NetworkMagic)
import           Pos.Crypto (EncryptedSecretKey)
import           Pos.Crypto.Signing.Safe (emptyPassphrase, safeKeyGen)
import           Pos.Util.Util (maybeThrow)
import           Pos.Wallet.Web.Account (AccountMode, getSKById)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), CAccountMeta (..), CId,
                                             CWalletMeta (..), Wal)
import           Pos.Wallet.Web.Error (WalletError (..))
import           Pos.Wallet.Web.State (WalletSnapshot, getAccountMeta, getWalletMeta)
import           Pos.Wallet.Web.Util (getWalletAccountIds)

currentBackupFormatVersion :: V.Version
currentBackupFormatVersion = V.initial & V.major .~ 1

newtype WalletMetaBackup = WalletMetaBackup CWalletMeta deriving Show
newtype AccountMetaBackup = AccountMetaBackup CAccountMeta deriving Show

data WalletBackup = WalletBackup
    { wbSecretKey :: !EncryptedSecretKey
    , wbMeta      :: !WalletMetaBackup
    , wbAccounts  :: !(HashMap Int AccountMetaBackup)
    } deriving Show

data TotalBackup = TotalBackup WalletBackup

instance Arbitrary WalletBackup where
    arbitrary = do
        cwNameT <- arbitrary
        caNameT <- arbitrary
        wbInt   <- arbitrary
        cwAssurance <- elements [minBound .. maxBound]
        (_, esk) <- safeKeyGen emptyPassphrase
        let cwUnit = 1
            cwName = (cwNameT :: Text )
            caName = (caNameT :: Text )
            wMetaBackup = WalletMetaBackup $ CWalletMeta {..}
        return $ WalletBackup
            { wbSecretKey = esk
            , wbMeta = wMetaBackup
            , wbAccounts = HM.singleton wbInt (AccountMetaBackup CAccountMeta {..})
            }

getWalletBackup :: AccountMode ctx m
                => NetworkMagic
                -> WalletSnapshot
                -> CId Wal
                -> m WalletBackup
getWalletBackup nm ws wId = do
    sk <- getSKById nm wId
    meta <- maybeThrow (InternalError "Wallet have no meta") $
            getWalletMeta ws wId
    let accountIds = getWalletAccountIds ws wId
    accountMetas <- forM accountIds $ \accid ->
        maybeThrow (InternalError "Account have no meta") $
        getAccountMeta ws accid

    let accountsMap = HM.fromList $ zip
            (map (fromInteger . fromIntegral . aiIndex) accountIds)
            (map AccountMetaBackup accountMetas)

    return WalletBackup
        { wbSecretKey = sk
        , wbMeta = WalletMetaBackup meta
        , wbAccounts = accountsMap
        }
