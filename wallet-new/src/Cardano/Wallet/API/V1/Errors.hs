{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Cardano.Wallet.API.V1.Errors where

import           Universum

import           Data.Aeson
import           Data.Aeson.TH (deriveJSON)
import qualified Network.HTTP.Types.Header as HTTP
import           Servant
import           Test.QuickCheck (Arbitrary (..))

import           Cardano.Wallet.API.V1.TH (conNamesList, deriveWalletErrorJSON)

--
-- Error handling
--

-- | Type representing any error which might be thrown by wallet.
-- TODO: change fields' types to actual Cardano core types, like `Coin` and `Address`
data WalletError =
      NotEnoughMoney { weNeedMore :: !Int }
    | OutputIsRedeem { weAddress :: !Text }
    | SomeOtherError { weFoo :: !Text, weBar :: !Int }
    | NotRecordError !Int !Text
    | MigrationFailed { weDescription :: !Text }
    | WalletNotFound
    deriving (Show, Eq)

deriveWalletErrorJSON ''WalletError

instance Exception WalletError where

-- | List of all existing error tags. Populates automatically
allErrorsList :: [Text]
allErrorsList = $(conNamesList ''WalletError)

-- | Function which determines which HTTP error corresponds to each
-- `WalletError`.
-- Note: current choices of particular errors are debatable
walletHTTPError :: WalletError -> ServantErr
walletHTTPError (NotEnoughMoney _)   = err403 -- <https://httpstatuses.com/403 403> Forbidden
walletHTTPError (OutputIsRedeem _)   = err403
walletHTTPError (SomeOtherError _ _) = err418 -- <https://httpstatuses.com/418 418> I'm a teapot
walletHTTPError (NotRecordError _ _) = err418
walletHTTPError (MigrationFailed _)  = err422 -- <https://httpstatuses.com/422 422> Unprocessable Entity
walletHTTPError WalletNotFound       = err404 -- <https://httpstatuses.com/404 404> NotFound

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
