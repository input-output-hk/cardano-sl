{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

module Cardano.Faucet.Types.API (
   WithdrawalRequest(..), wAddress, gRecaptchaResponse
 , WithdrawalResult(..), _WithdrawalError, _WithdrawalSuccess
 , GCaptchaResponse(..)
 , WithdrawalQFull(..)
  ) where

import           Control.Exception.Safe
import           Control.Lens (at, makeLenses, makePrisms, makeWrapped, mapped,
                     (?~), _Wrapped)
import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject,
                     (.:), (.=))
import qualified Data.Char as Char
import           Data.Proxy
import           Data.String (IsString (..))
import           Data.Swagger
import           Data.Text (Text)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           Test.QuickCheck.Arbitrary.Generic
import           Universum
import           Web.FormUrlEncoded

import           Cardano.Wallet.API.V1.Types (Transaction, V1 (..))
import           Pos.Core (Address (..))


--------------------------------------------------------------------------------
-- | The "g-recaptcha-response" field
newtype GCaptchaResponse = GCaptchaResponse Text deriving (Eq, Show, Generic)

makeWrapped ''GCaptchaResponse

instance IsString GCaptchaResponse where
    fromString = GCaptchaResponse . fromString

instance Arbitrary GCaptchaResponse where
    arbitrary = genericArbitrary
    shrink = genericShrink

--------------------------------------------------------------------------------
-- | A request to withdraw ADA from the faucet wallet
data WithdrawalRequest = WithdrawalRequest {
    -- | The address to send the ADA to
    _wAddress           :: !(V1 Address)
    -- | The "g-recaptcha-response" field sent by the form
  , _gRecaptchaResponse :: !GCaptchaResponse
  } deriving (Eq, Show, Typeable, Generic)

makeLenses ''WithdrawalRequest

instance FromJSON WithdrawalRequest where
  parseJSON = withObject "WithdrawalRequest" $ \v -> WithdrawalRequest
    <$> v .: "address"
    <*> (GCaptchaResponse <$> v .: "g-recaptcha-response")

instance FromForm WithdrawalRequest where
    fromForm f = WithdrawalRequest
      <$> parseUnique "address" f
      <*> (GCaptchaResponse <$> parseUnique "g-recaptcha-response" f)

instance ToSchema WithdrawalRequest where
    declareNamedSchema _ = do
        addrSchema <- declareSchemaRef (Proxy :: Proxy (V1 Address))
        recaptchaSchema <- declareSchemaRef (Proxy :: Proxy Text)
        return $ NamedSchema (Just "WithdrawalRequest") $ mempty
          & type_ .~ SwaggerObject
          & properties .~ (mempty & at "address" ?~ addrSchema
                                  & at "g-recaptcha-response" ?~ recaptchaSchema)
          & required .~ ["address", "g-recaptcha-response"]

instance ToJSON WithdrawalRequest where
    toJSON (WithdrawalRequest w g) =
        object [ "address" .= w
               , "g-recaptcha-response" .= (g ^. _Wrapped)]


instance Arbitrary WithdrawalRequest where
    arbitrary = genericArbitrary
    shrink = genericShrink

--------------------------------------------------------------------------------
data WithdrawalQFull = WithdrawalQFull deriving (Show, Generic, Exception)

instance ToJSON WithdrawalQFull where
  toJSON _ =
      object [ "error" .= ("Withdrawal queue is full" :: Text)
             , "status" .= ("error" :: Text) ]

instance ToSchema WithdrawalQFull where
    declareNamedSchema _ = do
        strSchema <- declareSchemaRef (Proxy :: Proxy Text)
        return $ NamedSchema (Just "WithdrawalQFull") $ mempty
          & type_ .~ SwaggerObject
          & properties .~ (mempty
              & at "status" ?~ strSchema
              & at "error" ?~ strSchema)
          & required .~ ["status"]

--------------------------------------------------------------------------------
data WithdrawalResult =
    WithdrawalError Text   -- ^ Error with http client error
  | WithdrawalSuccess Transaction -- ^ Success with transaction details
  deriving (Show, Typeable, Generic)

makePrisms ''WithdrawalResult

instance ToJSON WithdrawalResult where
    toJSON (WithdrawalSuccess txn) =
        object ["success" .= txn]
    toJSON (WithdrawalError err) =
        object ["error" .= err]

wdDesc :: Text
wdDesc = "An object with either a success field containing the transaction or "
      <> "an error field containing the ClientError from the wallet as a string"

instance ToSchema WithdrawalResult where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions
      { constructorTagModifier = map Char.toLower . drop (length ("Withdrawal" :: String)) }
      & mapped.mapped.schema.description ?~ wdDesc
