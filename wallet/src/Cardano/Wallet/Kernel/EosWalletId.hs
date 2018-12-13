{-# LANGUAGE DeriveGeneric #-}
-- TODO: Not sure about the best way to avoid the orphan instances here
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Kernel.EosWalletId (
    EosWalletId
  , genEosWalletId
  ) where

import           Prelude
import           Universum

import qualified Data.Aeson.Options as Aeson
import           Data.Aeson.TH
import           Data.SafeCopy (base, deriveSafeCopy)
import           Data.Swagger (ToSchema (..), defaultSchemaOptions,
                     genericDeclareNamedSchema)

import           Data.UUID (UUID)
import qualified Data.UUID as Uuid
import           Data.UUID.V4 (nextRandom)

import           Formatting (bprint, build, fconst, sformat)

import           Cardano.Wallet.API.V1.Swagger.Example (Example, example)

import           Pos.Infra.Util.LogSafe (BuildableSafeGen (..),
                     deriveSafeBuildable, plainOrSecureF)

import           Servant.API (FromHttpApiData (..), ToHttpApiData (..))
import           Test.QuickCheck (Arbitrary (..))
import qualified Test.QuickCheck.Gen as Gen

-- | Since EOS-wallet doesn't store root key, we cannot create 'RootId'
-- for it. So currently we use UUID to identity EOS-wallet.
--
-- Please note that UUID-based solution may change in the future.
newtype EosWalletId = EosWalletId { getEosWalletId :: UUID }
    deriving (Show, Eq, Ord, Generic)

instance ToSchema EosWalletId where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance Arbitrary EosWalletId where
    arbitrary = EosWalletId <$> Gen.chooseAny

deriveSafeBuildable ''EosWalletId
instance BuildableSafeGen EosWalletId where
    buildSafeGen sl (EosWalletId uuid) =
        bprint (plainOrSecureF sl build (fconst "<EOS-wallet id>")) $ Uuid.toText uuid

instance FromHttpApiData EosWalletId where
    parseQueryParam rawUuid = case Uuid.fromText rawUuid of
        Nothing   -> Left "Invalid EOS-wallet id (not a UUID)."
        Just uuid -> Right . EosWalletId $ uuid

instance ToHttpApiData EosWalletId where
    toQueryParam uuid = sformat build uuid

instance Example EosWalletId where
    example = EosWalletId <$>
        pure (Prelude.read "c2cc10e1-57d6-4b6f-9899-38d972112d8c")

deriveJSON Aeson.defaultOptions ''EosWalletId

-- | Generator for new 'EosWalletId'.
genEosWalletId :: IO EosWalletId
genEosWalletId = EosWalletId <$> nextRandom

-- | For 'acid-state'. We define it here to hide 'UUID' in this module only.
deriveSafeCopy 1 'base ''UUID
deriveSafeCopy 1 'base ''EosWalletId
