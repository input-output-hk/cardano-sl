{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Cardano.Wallet.API.V1.Errors where

import           Universum

import           Data.Aeson
import           Generics.SOP.TH (deriveGeneric)
import qualified Network.HTTP.Types.Header as HTTP
import           Servant
import           Test.QuickCheck (Arbitrary (..), oneof)

import           Cardano.Wallet.API.V1.Generic (gconsNames, gparseJsend, gtoJsend)

--
-- Error handling
--

-- | Type representing any error which might be thrown by wallet.
--
-- Errors are represented in JSON in the JSend format (<https://labs.omniti.com/labs/jsend>):
-- ```
-- {
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
    | WalletNotFound
    deriving (Show, Eq)

--
-- Instances for `WalletError`

-- deriveWalletErrorJSON ''WalletError
deriveGeneric ''WalletError

instance ToJSON WalletError where
    toJSON = gtoJsend

instance FromJSON WalletError where
    parseJSON = gparseJsend

instance Exception WalletError where

-- TODO: generate `Arbitrary` instance with TH too?
instance Arbitrary WalletError where
    arbitrary = oneof
        [ NotEnoughMoney <$> arbitrary
        , OutputIsRedeem <$> pure "address"
        , SomeOtherError <$> pure "blah" <*> arbitrary
        , MigrationFailed <$> pure "migration"
        , pure WalletNotFound
        ]

--
-- Helpers
--

-- | List of all existing error tags. Populates automatically
allErrorsList :: [Text]
allErrorsList = gconsNames (Proxy :: Proxy WalletError)

-- | Function which determines which HTTP error corresponds to each
-- `WalletError`.
-- Note: current choices of particular errors are debatable
walletHTTPError :: WalletError -> ServantErr
walletHTTPError NotEnoughMoney{}  = err403 -- <https://httpstatuses.com/403 403> Forbidden
walletHTTPError OutputIsRedeem{}  = err403
walletHTTPError SomeOtherError{}  = err418 -- <https://httpstatuses.com/418 418> I'm a teapot
walletHTTPError MigrationFailed{} = err422 -- <https://httpstatuses.com/422 422> Unprocessable Entity
walletHTTPError WalletNotFound    = err404 -- <https://httpstatuses.com/404 404> NotFound

-- | "Hoist" the given 'Wallet' error into a 'ServantError',
-- returning as the response body the encoded JSON representation
-- of the error.
toError :: WalletError -> ServantErr
toError we = let err@ServantErr{..} = walletHTTPError we in
    err { errBody = encode we
        , errHeaders = applicationJson : errHeaders
        }

-- | Generates the @Content-Type: application/json@ 'HTTP.Header'.
applicationJson :: HTTP.Header
applicationJson =
    let [hdr] = getHeaders (addHeader "application/json" mempty :: (Headers '[Header "Content-Type" String] String))
    in hdr
