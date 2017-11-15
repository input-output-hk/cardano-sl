{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Cardano.Wallet.API.V1.Errors where

import           Universum

import           Data.Aeson
import           Data.Aeson.TH (deriveJSON)
import           Test.QuickCheck (Arbitrary (..))

import           Cardano.Wallet.API.V1.TH (conNamesList, deriveWalletErrorJSON)

--
-- Error handling
--

-- | "Hoist" the given 'Wallet' error into a 'ServantError',
-- returning as the response body the encoded JSON representation
-- of the error.
toError :: ServantErr -> WalletError -> ServantErr
toError err@ServantErr{..} we =
    err { errBody = encode we
        , errHeaders = applicationJson : errHeaders
        }

-- | Generates the @Content-Type: application/json@ 'HTTP.Header'.
applicationJson :: HTTP.Header
applicationJson =
    let [hdr] = getHeaders (addHeader "application/json" mempty :: (Headers '[Header "Content-Type" String] String))
    in hdr

-- | Type representing any error which might be thrown by wallet.
-- TODO: change fields' types to actual Cardano core types, like `Coin` and `Address`
data WalletError =
      NotEnoughMoney { weNeedMore :: !Int }
    | OutputIsRedeem { weAddress :: !Text }
    | SomeOtherError { weFoo :: !Text, weBar :: !Int }
    | NotRecordError !Int !Text
    | WalletNotFound

deriveWalletErrorJSON ''WalletError

allErrorsList :: [Text]
allErrorsList = $(conNamesList ''WalletError)
