{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Aeson.WalletBackup
       (
       ) where

import           Universum

import           Data.Aeson                 (FromJSON (..), ToJSON (..), Value (..),
                                             object, withArray, withObject, (.:), (.=))
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.HashMap.Strict        as HM
import           Formatting                 (formatToString, stext, (%))
import qualified Serokell.Util.Base64       as B64

import qualified Pos.Binary                 as Bi
import           Pos.Crypto                 (EncryptedSecretKey (..))
import           Pos.Util.Util              (eitherToFail)
import           Pos.Wallet.Web.Backup      (AccountMetaBackup (..), StateBackup (..),
                                             WalletBackup (..), WalletMetaBackup (..),
                                             currentBackupFormatVersion)
import           Pos.Wallet.Web.ClientTypes (CAccountMeta (..), CWalletAssurance (..),
                                             CWalletMeta (..))

data IndexedAccountMeta = IndexedAccountMeta Int AccountMetaBackup

strToUnit :: MonadFail m => Text -> m Int
strToUnit "ADA"      = pure 0
strToUnit "Lovelace" = pure 1
strToUnit s          = fail $ "Unknown wallet unit: " ++ toString s

strToAssurance :: MonadFail m => Text -> m CWalletAssurance
strToAssurance "normal" = pure CWANormal
strToAssurance "strict" = pure CWAStrict
strToAssurance s        = fail $ "Unknown assurance type: " ++ toString s

unitToStr :: Int -> Text
unitToStr 0 = "ADA"
unitToStr 1 = "Lovelace"
unitToStr _ = error "Units >1 are not currently used in Cardano!"

assuranceToStr :: CWalletAssurance -> Text
assuranceToStr CWANormal = "normal"
assuranceToStr CWAStrict = "strict"

checkIfCurrentVersion :: MonadFail m => Text -> m ()
checkIfCurrentVersion version
    | version == currentBackupFormatVersion = pure ()
    | otherwise =
          fail $ formatToString
          ("Unsupported backup format version "%stext%", expected "%stext)
          version currentBackupFormatVersion

instance FromJSON AccountMetaBackup where
    parseJSON = withObject "AccountMetaBackup" $ \o -> do
        caName <- o .: "name"
        return $ AccountMetaBackup $ CAccountMeta {..}

instance FromJSON WalletMetaBackup where
    parseJSON = withObject "WalletMetaBackup" $ \o -> do
        cwName <- o .: "name"
        cwAssurance <- strToAssurance =<< o .: "assurance"
        cwUnit <- strToUnit =<< o .: "unit"
        return $ WalletMetaBackup $ CWalletMeta {..}

instance FromJSON IndexedAccountMeta where
    parseJSON = withObject "IndexedAccountMeta" $ \o -> do
        idx <- o .: "index"
        meta <- parseJSON $ Object o
        return $ IndexedAccountMeta idx meta

instance FromJSON WalletBackup where
    parseJSON = withObject "WalletBackup" $ \o -> do
        let decodeBase64 x = eitherToFail (B64.decode x) >>= eitherToFail . Bi.decodeFull . BSL.fromStrict
            collectAccMap = foldlM parseAddAcc HM.empty
            parseAddAcc accMap v = do
                IndexedAccountMeta idx meta <- parseJSON v
                return $ HM.insert idx meta accMap

        prvKey <- decodeBase64 =<< o .: "walletSecretKey"
        passPhraseHash <- decodeBase64 =<< o .: "passwordHash"
        walletMeta <- o .: "walletMeta"
        walletAccounts <- withArray "WalletBackup.accounts" collectAccMap =<<
                          o .: "accounts"
        let encKey = EncryptedSecretKey prvKey passPhraseHash
        return $ WalletBackup encKey walletMeta walletAccounts

instance FromJSON StateBackup where
    parseJSON = withObject "StateBackup" $ \o -> do
        fileType :: Text <- o .: "fileType"
        case fileType of
            "WALLETS_EXPORT" -> do
                o .: "fileVersion" >>= checkIfCurrentVersion
                FullStateBackup <$> o .: "wallets"
            unknownType -> fail $ "Unknown type of backup file: " ++ toString unknownType

instance ToJSON AccountMetaBackup where
    toJSON (AccountMetaBackup (CAccountMeta {..})) =
        object ["name" .= caName]

instance ToJSON WalletMetaBackup where
    toJSON (WalletMetaBackup (CWalletMeta {..})) = object
        [ "name" .= cwName
        , "assurance" .= assuranceToStr cwAssurance
        , "unit" .= unitToStr cwUnit
        ]

instance ToJSON IndexedAccountMeta where
    toJSON (IndexedAccountMeta idx meta) =
        case toJSON meta of
            Object v -> Object $ HM.insert "index" (toJSON idx) v
            _        -> error "Account metadata isn't encoded as JSON object"

instance ToJSON WalletBackup where
    toJSON (WalletBackup skey wMeta wAccounts) = object
        [ "walletSecretKey" .= B64.encode (Bi.encodeStrict prvKey)
        , "passwordHash" .= B64.encode (Bi.encodeStrict passPhraseHash)
        , "walletMeta" .= wMeta
        , "accounts" .= encodeAccMap wAccounts
        ]
      where
        EncryptedSecretKey prvKey passPhraseHash = skey
        encodeAccMap = toJSON . map (uncurry IndexedAccountMeta) . HM.toList

instance ToJSON StateBackup where
    toJSON (FullStateBackup wallets) = object
        [ "fileType" .= ("WALLETS_EXPORT" :: Text)
        , "fileVersion" .= currentBackupFormatVersion
        , "wallets" .= wallets
        ]
