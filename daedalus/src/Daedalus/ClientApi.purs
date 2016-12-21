module Daedalus.ClientApi where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Promise (Promise, fromAff)
import Daedalus.BackendApi as B
import Data.Tuple (Tuple)
import Network.HTTP.Affjax (AJAX)
import Daedalus.Types (mkCAddress, mkCoin, CAddress, Coin)

getWallets :: forall eff. Eff(ajax :: AJAX | eff) (Promise (Array CAddress))
getWallets = fromAff B.getAddresses

getBalances :: forall eff. Eff(ajax :: AJAX | eff) (Promise (Array (Tuple CAddress Coin)))
getBalances = fromAff B.getBalances

-- getHistory :: forall eff. String -> Eff(ajax :: AJAX | eff) (Promise (Array Tx)))
-- getHistory = fromAff <<< B.getBalances <<< mkCAddress

send :: forall eff. String -> String -> Int -> Eff(ajax :: AJAX | eff) (Promise Unit)
send addrTo addrFrom amount = fromAff $
    B.send
        (mkCAddress addrTo)
        (mkCAddress addrFrom)
        (mkCoin amount)

newWallet :: forall eff. Eff(ajax :: AJAX | eff) (Promise CAddress)
newWallet = fromAff B.newAddress

deleteWallet :: forall eff. String -> Eff(ajax :: AJAX | eff) (Promise Unit)
deleteWallet = fromAff <<< B.deleteAddress <<< mkCAddress
