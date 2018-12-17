module Cardano.Wallet.API.V1.Headers
    ( applicationJson
    ) where

import           Network.HTTP.Types (Header, hContentType)


-- | Generates the @Content-Type: application/json@ 'HTTP.Header'.
applicationJson :: Header
applicationJson =
    (hContentType, "application/json")
