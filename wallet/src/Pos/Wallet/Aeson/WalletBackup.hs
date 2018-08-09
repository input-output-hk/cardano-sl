{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pos.Wallet.Aeson.WalletBackup
       (
       ) where

import           Universum

import           Control.Lens (_Left)
import           Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withArray, withObject,
                             withText, (.:), (.=))
import qualified Data.HashMap.Strict as HM
import qualified Data.SemVer as V
import           Formatting (build, sformat, (%))
import qualified Serokell.Util.Base64 as B64

import qualified Pos.Binary as Bi
import           Pos.Crypto (EncryptedSecretKey (..), SecretKey (..))
import           Pos.Util.Util (aesonError, toAesonError)
import           Pos.Wallet.Web.Backup (AccountMetaBackup (..), TotalBackup (..), WalletBackup (..),
                                        WalletMetaBackup (..), currentBackupFormatVersion)
import           Pos.Wallet.Web.ClientTypes (CAccountMeta (..), CWalletAssurance (..),
                                             CWalletMeta (..))

data IndexedAccountMeta = IndexedAccountMeta Int AccountMetaBackup

strToUnit :: Text -> Either Text Int
strToUnit "ADA"      = Right 0
strToUnit "Lovelace" = Right 1
strToUnit s          = Left $ "Unknown wallet unit: " <> s

strToAssurance :: Text -> Either Text CWalletAssurance
strToAssurance "normal" = Right CWANormal
strToAssurance "strict" = Right CWAStrict
strToAssurance s        = Left $ "Unknown assurance type: " <> s

unitToStr :: Int -> Text
unitToStr 0 = "ADA"
unitToStr 1 = "Lovelace"
unitToStr _ = error "Units >1 are not currently used in Cardano!"

assuranceToStr :: CWalletAssurance -> Text
assuranceToStr CWANormal = "normal"
assuranceToStr CWAStrict = "strict"

checkIfCurrentVersion :: V.Version -> Either Text ()
checkIfCurrentVersion version
    | version == currentBackupFormatVersion = Right ()
    | otherwise =
          Left $ sformat
          ("Unsupported backup format version "%build%", expected "%build)
          (V.toBuilder version) (V.toBuilder currentBackupFormatVersion)


instance FromJSON V.Version where
    parseJSON = withText "Version" $
        toAesonError . over _Left fromString . V.fromText

instance FromJSON AccountMetaBackup where
    parseJSON = withObject "AccountMetaBackup" $ \o -> do
        caName <- o .: "name"
        return $ AccountMetaBackup $ CAccountMeta {..}

instance FromJSON WalletMetaBackup where
    parseJSON = withObject "WalletMetaBackup" $ \o -> do
        cwName <- o .: "name"
        cwAssurance <- toAesonError . strToAssurance =<< o .: "assurance"
        cwUnit <- toAesonError . strToUnit =<< o .: "unit"
        return $ WalletMetaBackup $ CWalletMeta {..}

instance FromJSON IndexedAccountMeta where
    parseJSON = withObject "IndexedAccountMeta" $ \o -> do
        idx <- o .: "index"
        meta <- parseJSON $ Object o
        return $ IndexedAccountMeta idx meta

instance FromJSON WalletBackup where
    parseJSON = withObject "WalletBackup" $ \o -> do
        let decodeBase64 x = toAesonError (B64.decode x) >>= toAesonError . Bi.decodeFull'
            collectAccMap = foldlM parseAddAcc HM.empty
            parseAddAcc accMap v = do
                IndexedAccountMeta idx meta <- parseJSON v
                return $ HM.insert idx meta accMap

        SecretKey prvKey <- decodeBase64 =<< o .: "walletSecretKey"
        passPhraseHash <- decodeBase64 =<< o .: "passwordHash"
        walletMeta <- o .: "walletMeta"
        walletAccounts <- withArray "WalletBackup.accounts" collectAccMap =<<
                          o .: "accounts"
        let encKey = EncryptedSecretKey prvKey passPhraseHash
        return $ WalletBackup encKey walletMeta walletAccounts

instance FromJSON TotalBackup where
    parseJSON = withObject "StateBackup" $ \o -> do
        fileType :: Text <- o .: "fileType"
        case fileType of
            "WALLETS_EXPORT" -> do
                o .: "fileVersion" >>= toAesonError . checkIfCurrentVersion
                TotalBackup <$> o .: "wallet"
            unknownType -> aesonError $ "Unknown type of backup file: " <> unknownType


instance ToJSON V.Version where
    toJSON = String . V.toText

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
        [ "walletSecretKey" .= B64.encode (Bi.serialize' $ SecretKey prvKey)
        , "passwordHash" .= B64.encode (Bi.serialize' passPhraseHash)
        , "walletMeta" .= wMeta
        , "accounts" .= encodeAccMap wAccounts
        ]
      where
        EncryptedSecretKey prvKey passPhraseHash = skey
        encodeAccMap = toJSON . map (uncurry IndexedAccountMeta) . HM.toList

instance ToJSON TotalBackup where
    toJSON (TotalBackup wallet) = object
        [ "fileType" .= ("WALLETS_EXPORT" :: Text)
        , "fileVersion" .= currentBackupFormatVersion
        , "wallet" .= wallet
        ]
