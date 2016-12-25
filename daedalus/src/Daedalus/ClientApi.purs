module Daedalus.ClientApi where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Promise (Promise, fromAff)
import Daedalus.BackendApi as B
import Data.Tuple (Tuple)
import Network.HTTP.Affjax (AJAX)
import Daedalus.Types (mkCAddress, mkCoin, CAddress, Coin, CWallet, CTx)

getWallets :: forall eff. Eff(ajax :: AJAX | eff) (Promise (Array CWallet))
getWallets = fromAff B.getWallets

getWallet :: forall eff. String -> Eff(ajax :: AJAX | eff) (Promise CWallet)
getWallet = fromAff <<< B.getWallet <<< mkCAddress

getHistory :: forall eff. String -> Eff(ajax :: AJAX | eff) (Promise (Array CTx))
getHistory = fromAff <<< B.getHistory <<< mkCAddress

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
