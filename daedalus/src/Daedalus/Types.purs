module Daedalus.Types
       ( module CT
       , module T
       , _address
       , _coin
       , mkCoin
       , mkCAddress
       ) where

import Prelude

import Pos.Wallet.Web.ClientTypes (CAddress (..), CHash (..))
import Pos.Types.Types (Coin (..))

import Pos.Wallet.Web.ClientTypes as CT
import Pos.Types.Types as T

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
