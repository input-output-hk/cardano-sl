module Daedalus.Types
       ( module CT
       , module T
       , _address
       , _coin
       , mkCoin
       , mkCAddress
       , mkCWalletMeta
       , _ctxIdValue
       ) where

import Prelude

import Pos.Wallet.Web.ClientTypes (CAddress (..), CHash (..))
import Pos.Types.Types (Coin (..))

import Pos.Wallet.Web.ClientTypes as CT
import Pos.Types.Types as T

import Data.Either (either)
import Data.Argonaut.Generic.Aeson (decodeJson)
import Data.Argonaut.Core (fromString)

-- TODO: it would be useful to extend purescript-bridge
-- and generate lenses
_address :: CAddress -> String
_address (CAddress (CHash s)) = s

mkCAddress :: String -> CAddress
mkCAddress = CAddress <<< CHash

_coin :: Coin -> Int
_coin (Coin c) = c.getCoin

mkCoin :: Int -> Coin
mkCoin amount = Coin { getCoin: amount }

-- NOTE: use genericRead maybe https://github.com/paluh/purescript-generic-read-example
mkCWalletType :: String -> CT.CWalletType
mkCWalletType = either (const CT.CWTPersonal) id <<< decodeJson <<< fromString

mkCCurrency :: String -> CT.CCurrency
mkCCurrency = either (const CT.ADA) id <<< decodeJson <<< fromString

mkCWalletMeta :: String -> String -> String -> CT.CWalletMeta
mkCWalletMeta wType wCurrency wName =
    CT.CWalletMeta { cwType: mkCWalletType wType
                , cwCurrency: mkCCurrency wCurrency
                , cwName: wName
                }

_ctxIdValue :: CT.CTxId -> String
_ctxIdValue (CT.CTxId (CT.CHash h)) = h
