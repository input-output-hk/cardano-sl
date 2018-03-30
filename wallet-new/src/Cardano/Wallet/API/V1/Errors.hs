{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Cardano.Wallet.API.V1.Errors where

import           Universum

import           Cardano.Wallet.API.Response.JSend (ResponseStatus (ErrorStatus))
import           Data.Aeson
import           Data.List.NonEmpty (NonEmpty)
import           Generics.SOP.TH (deriveGeneric)
import qualified Network.HTTP.Types.Header as HTTP
import           Servant
import           Test.QuickCheck (Arbitrary (..), oneof)

import           Cardano.Wallet.API.V1.Generic (gparseJsend, gtoJsend)

--
-- Error handling
--

-- | Type representing any error which might be thrown by wallet.
--
-- Errors are represented in JSON in the JSend format (<https://labs.omniti.com/labs/jsend>):
-- ```
-- {
--     "status": "error"
--     "message" : <constr_name>,
--     "diagnostic" : <data>
-- }
-- ```
-- where `<constr_name>` is a string containing name of error's constructor (e. g. `NotEnoughMoney`),
-- and `<data>` is an object containing additional error data.
-- Additional data contains constructor fields, field names are record field names without
-- a `we` prefix, e. g. for `OutputIsRedeem` error "diagnostic" field will be the following:
-- ```
-- {
--     "address" : <address>
-- }
-- ```
--
-- Additional data in constructor should be represented as record fields.
-- Otherwise TemplateHaskell will raise an error.
--
-- If constructor does not have additional data (like in case of `WalletNotFound` error),
-- then "diagnostic" field will be empty object.
--
-- TODO: change fields' types to actual Cardano core types, like `Coin` and `Address`
data WalletError =
      NotEnoughMoney { weNeedMore :: !Int }
    | OutputIsRedeem { weAddress :: !Text }
    | SomeOtherError { weFoo :: !Text, weBar :: !Int }
    | MigrationFailed { weDescription :: !Text }
    | JSONValidationFailed { weValidationError :: !Text }
    | UnkownError { weMsg :: !Text }
    | InvalidAddressFormat { weMsg :: !Text }
    | WalletNotFound
    | AddressNotFound
    | MissingRequiredParams { requiredParams :: NonEmpty (Text, Text) }
    deriving (Show, Eq)

--
-- Instances for `WalletError`

-- deriveWalletErrorJSON ''WalletError
deriveGeneric ''WalletError

instance ToJSON WalletError where
    toJSON = gtoJsend ErrorStatus

instance FromJSON WalletError where
    parseJSON = gparseJsend

instance Exception WalletError where

-- TODO: generate `Arbitrary` instance with TH too?
instance Arbitrary WalletError where
    arbitrary = oneof (map pure sample)

--
-- Helpers
--

type ErrorName = Text
type ErrorCode = Int
type ErrorExample = Value


-- | Sample of errors we use for documentation
sample :: [WalletError]
sample =
  [ NotEnoughMoney 1400
  , OutputIsRedeem "b10b24203f1f0cadffcfd16277125cf7f3ad598983bef9123be80d93"
  , SomeOtherError "foo" 14
  , MigrationFailed "migration"
  , JSONValidationFailed "Expected String, found Null."
  , UnkownError "unknown"
  , WalletNotFound
  ]


-- | Convert wallet errors to Servant errors
toServantError :: WalletError -> ServantErr
toServantError err =
  mkServantErr $ case err of
    NotEnoughMoney{}       -> err403
    OutputIsRedeem{}       -> err403
    SomeOtherError{}       -> err418
    MigrationFailed{}      -> err422
    JSONValidationFailed{} -> err400
    UnkownError{}          -> err400
    WalletNotFound{}       -> err404
    InvalidAddressFormat{} -> err401
    AddressNotFound{}      -> err404
    MissingRequiredParams{} -> err400
  where
    mkServantErr serr@ServantErr{..} = serr
      { errBody    = encode err
      , errHeaders = applicationJson : errHeaders
      }

-- | Generates the @Content-Type: application/json@ 'HTTP.Header'.
applicationJson :: HTTP.Header
applicationJson =
    let [hdr] = getHeaders (addHeader "application/json" mempty :: (Headers '[Header "Content-Type" String] String))
    in hdr
