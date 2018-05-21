{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pos.Wallet.Aeson.ClientTypes
       (
       ) where

import           Universum

import           Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withArray, withObject,
                             (.:), (.=))
import           Data.Aeson.TH (defaultOptions, deriveJSON, deriveToJSON)
import           Data.Aeson.Types (Parser, typeMismatch)
import           Data.Version (showVersion)
import           Servant.API.ContentTypes (NoContent (..))

import           Pos.Client.Txp.Util (InputSelectionPolicy (..))
import           Pos.Util.BackupPhrase (BackupPhrase)
import           Pos.Util.Util (aesonError)
import           Pos.Wallet.Web.ClientTypes (Addr, ApiVersion (..), CAccount, CAccountId,
                                             CAccountInit, CAccountMeta, CAddress, CCoin,
                                             CFilePath (..), CHash, CId, CInitialized,
                                             CPaperVendWalletRedeem, CProfile, CPtxCondition,
                                             CTExMeta, CTx, CTxId, CTxMeta, CUpdateInfo,
                                             CWAddressMeta, CWallet, CWalletAssurance, CWalletInit,
                                             CWalletMeta, CWalletRedeem, ClientInfo (..),
                                             NewBatchPayment (..), SyncProgress, Wal)
import           Pos.Wallet.Web.Error (WalletError)
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

-- NOTE(adinapoli): We need a manual instance to ensure we map @OptimizeForSize@
-- to @OptimizeForHighThroughput@, for exchanges backward compatibility.
instance ToJSON InputSelectionPolicy where
  toJSON pol =
      let renderPolicy = toJSON @Text $ case pol of
              OptimizeForSecurity       -> "OptimizeForSecurity"
              OptimizeForHighThroughput -> "OptimizeForHighThroughput"
          in object [ "groupingPolicy" .= renderPolicy ]

-- NOTE(adinapoli): Super lenient decoder to overcome the fact some revisions of
-- the API were using a raw JSON String and later versions used on Object like
-- `{"groupingPolicy": "blabla" }`
instance FromJSON InputSelectionPolicy where
    parseJSON (Object o)         = fromRawPolicy =<< (o .: "groupingPolicy")
    parseJSON (String rawPolicy) = fromRawPolicy rawPolicy
    parseJSON x                  = typeMismatch "Not a valid InputSelectionPolicy" x

fromRawPolicy :: Text -> Parser InputSelectionPolicy
fromRawPolicy rawPolicy = case rawPolicy of
    "OptimizeForSecurity"       -> pure OptimizeForSecurity
    "OptimizeForHighThroughput" -> pure OptimizeForHighThroughput
    "OptimiseForSize"           -> pure OptimizeForHighThroughput
    _                           -> typeMismatch "Not a valid InputSelectionPolicy" (String rawPolicy)


deriveJSON defaultOptions ''CCoin
deriveJSON defaultOptions ''CTxId
deriveJSON defaultOptions ''CAddress
deriveJSON defaultOptions ''CAccount
deriveJSON defaultOptions ''CWallet
deriveJSON defaultOptions ''CPtxCondition
deriveJSON defaultOptions ''CTx
deriveJSON defaultOptions ''CTExMeta
deriveJSON defaultOptions ''CUpdateInfo
deriveJSON defaultOptions ''WalletError
deriveJSON defaultOptions ''SyncProgress

deriveToJSON defaultOptions ''NotifyEvent

-- For backward compatibility.
-- Guys /really/ want it to be normal JSON
deriving instance FromJSON CFilePath

instance ToJSON ApiVersion where
    toJSON ApiVersion0 = "v0"

instance ToJSON ClientInfo where
    toJSON ClientInfo {..} =
        object
            [ "gitRevision" .= ciGitRevision
            , "softwareVersion" .= pretty ciSoftwareVersion
            , "cabalVersion" .= showVersion ciCabalVersion
            , "apiVersion" .= ciApiVersion
            ]

instance ToJSON NoContent where
    toJSON NoContent = toJSON ()

instance FromJSON NewBatchPayment where
    parseJSON = withObject "NewBatchPayment" $ \o -> do
        npbFrom <- o .: "from"
        npbTo <-
            (`whenNothing` expectedOneRecipient) . nonEmpty . toList =<<
            withArray "NewBatchPayment.to" collectRecipientTuples =<<
            o .: "to"
        npbInputSelectionPolicy <- o .: "policy"
        return NewBatchPayment {..}
      where
        expectedOneRecipient = aesonError "Expected at least one recipient."
        collectRecipientTuples = mapM $ withObject "NewBatchPayment.to[x]" $
            \o -> (,)
                <$> o .: "address"
                <*> o .: "amount"

instance ToJSON NewBatchPayment where
    toJSON NewBatchPayment {..} =
        object
            [ "from" .= toJSON npbFrom
            , "to" .= map toRecipient (toList npbTo)
            , "policy" .= npbInputSelectionPolicy
            ]
      where
        toRecipient (address, amount) =
            object
                [ "address" .= address
                , "amount" .= amount
                ]
