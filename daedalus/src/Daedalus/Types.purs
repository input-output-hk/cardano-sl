module Daedalus.Types
       ( module CT
       , module T
       , _address
       , _coin
       , mkCoin
       , mkCAddress
       , mkCWalletMeta
       , mkCTxMeta
       , mkCTxId
       , mkCCurrency
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

import Data.Types (mkTime, NominalDiffTime)

-- TODO: it would be useful to extend purescript-bridge
-- and generate lenses

_hash :: CHash -> String
_hash (CHash h) = h

_address :: CAddress -> String
_address (CAddress a) = _hash a

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
_ctxIdValue (CT.CTxId tx) = _hash tx

mkCTxId :: String -> CT.CTxId
mkCTxId = CT.CTxId <<< CHash

mkCTxMeta :: String -> String -> String -> Number -> CT.CTxMeta
mkCTxMeta currency title description date =
    CT.CTxMeta { ctmCurrency: mkCCurrency currency
               , ctmTitle: title
               , ctmDescription: description
               , ctmDate: mkTime date
               }
