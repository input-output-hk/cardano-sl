module Daedalus.ClientApi where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Promise (Promise, fromAff)
import Daedalus.BackendApi as B
import Data.Tuple (Tuple)
import Network.HTTP.Affjax (AJAX)
import Daedalus.Types (mkCAddress, mkCoin, CAddress, Coin, CWallet, CTx, mkCWalletMeta)
import Data.Argonaut.Generic.Aeson (encodeJson)

getWallets :: forall eff. Eff(ajax :: AJAX | eff) (Promise String)
getWallets = fromAff $ map (show <<< encodeJson) B.getWallets

getWallet :: forall eff. String -> Eff(ajax :: AJAX | eff) (Promise String)
getWallet = fromAff <<< map (show <<< encodeJson) <<< B.getWallet <<< mkCAddress

getHistory :: forall eff. String -> Eff(ajax :: AJAX | eff) (Promise String)
getHistory = fromAff <<< map (show <<< encodeJson) <<< B.getHistory <<< mkCAddress

send :: forall eff. String -> String -> Int -> Eff(ajax :: AJAX | eff) (Promise String)
send addrFrom addrTo amount = fromAff <<< map (show <<< encodeJson) $
    B.send
        (mkCAddress addrFrom)
        (mkCAddress addrTo)
        (mkCoin amount)

newWallet :: forall eff. String -> String -> String -> Eff(ajax :: AJAX | eff) (Promise String)
newWallet wType wCurrency wName = fromAff <<< map (show <<< encodeJson) <<< B.newWallet $ mkCWalletMeta wType wCurrency wName

updateWallet :: forall eff. String -> String -> String -> String -> Eff(ajax :: AJAX | eff) (Promise String)
updateWallet addr wType wCurrency wName = fromAff <<< map (show <<< encodeJson) <<<
    B.updateWallet
        (mkCAddress addr)
        $ mkCWalletMeta wType wCurrency wName
--
-- updateTransaction :: forall eff. String -> String -> String -> String -> String -> NominalDateTime -> Eff(ajax :: AJAX | eff) (Promise CWallet)
-- updateTransaction addr ctxId ctmCurrency ctmTitle ctmDescription ctmDate = fromAff <<<
--     B.updateTransaction
--         (mkCAddress addr)
--         (mkCTxId ctxId)
--         $ mkCWalletMeta wType wCurrency wName
--
deleteWallet :: forall eff. String -> Eff(ajax :: AJAX | eff) (Promise Unit)
deleteWallet = fromAff <<< B.deleteWallet <<< mkCAddress
