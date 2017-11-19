
module Cardano.Wallet.API.V1.Errors where

import           Universum

import           Cardano.Wallet.API.V1.Types (WalletError (..))
import           Data.Aeson (encode)
import qualified Network.HTTP.Types.Header as HTTP
import           Servant


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


walletNotFound :: WalletError
walletNotFound = WalletError {
      errCode = 600
    , errMessage = "The requested Wallet cannot be found."
    }
