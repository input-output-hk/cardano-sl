module Pos.Aeson.ClientTypes
       (
       ) where

import           Universum

import           Data.Aeson                   (FromJSON (..), ToJSON (..), object,
                                               withArray, withObject, (.:), (.=))
import           Data.Aeson.TH                (defaultOptions, deriveJSON, deriveToJSON)
import           Pos.Core.Types               (SoftwareVersion (..))
import           Pos.Util.BackupPhrase        (BackupPhrase)
import           Pos.Wallet.Web.ClientTypes   (Addr, CAccount, CAccountId, CAccountInit,
                                               CAccountMeta, CAddress, CCoin, CHash, CId,
                                               CInitialized, CInitialized,
                                               CPaperVendWalletRedeem, CProfile, CProfile,
                                               CPtxCondition, CTExMeta, CTx, CTxId,
                                               CTxMeta, CUpdateInfo, CWAddressMeta,
                                               CWallet, CWalletAssurance, CWalletInit,
                                               CWalletMeta, CWalletRedeem,
                                               NewBatchPayment (..), SyncProgress, Wal)
import           Pos.Wallet.Web.Error         (WalletError)
import           Pos.Wallet.Web.Sockets.Types (NotifyEvent)

deriveJSON defaultOptions ''CAccountId
deriveJSON defaultOptions ''CWAddressMeta
deriveJSON defaultOptions ''CWalletAssurance
deriveJSON defaultOptions ''CAccountMeta
deriveJSON defaultOptions ''CAccountInit
deriveJSON defaultOptions ''CWalletRedeem
deriveJSON defaultOptions ''CWalletMeta
deriveJSON defaultOptions ''CWalletInit
deriveJSON defaultOptions ''CPaperVendWalletRedeem
deriveJSON defaultOptions ''CTxMeta
deriveJSON defaultOptions ''CProfile
deriveJSON defaultOptions ''BackupPhrase
deriveJSON defaultOptions ''CId
deriveJSON defaultOptions ''Wal
deriveJSON defaultOptions ''Addr
deriveJSON defaultOptions ''CHash
deriveJSON defaultOptions ''CInitialized

deriveJSON defaultOptions ''CCoin
deriveJSON defaultOptions ''CTxId
deriveJSON defaultOptions ''CAddress
deriveJSON defaultOptions ''CAccount
deriveJSON defaultOptions ''CWallet
deriveJSON defaultOptions ''CPtxCondition
deriveJSON defaultOptions ''CTx
deriveJSON defaultOptions ''CTExMeta
deriveJSON defaultOptions ''SoftwareVersion
deriveJSON defaultOptions ''CUpdateInfo

deriveToJSON defaultOptions ''SyncProgress
deriveToJSON defaultOptions ''NotifyEvent
deriveToJSON defaultOptions ''WalletError

instance FromJSON NewBatchPayment where
    parseJSON = withObject "NewBatchPayment" $ \o -> do
        npbFrom <- o .: "from"
        npbTo <- (`whenNothing` expectedOneRecipient) . nonEmpty . toList =<< withArray "NewBatchPayment.to" collectRecipientTuples =<< o .: "to"
        return $ NewBatchPayment {..}
      where
        expectedOneRecipient = fail $ "Expected at least one recipient."
        collectRecipientTuples = mapM $ withObject "NewBatchPayment.to[x]" $
            \o -> (,)
                <$> o .: "address"
                <*> o .: "amount"

instance ToJSON NewBatchPayment where
    toJSON NewBatchPayment {..} =
        object
            [ "from" .= toJSON npbFrom
            , "to" .= map toRecipient (toList npbTo)
            ]
      where
        toRecipient (address, amount) =
            object
                [ "address" .= address
                , "amount" .= amount
                ]
