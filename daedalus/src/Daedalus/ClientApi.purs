module Daedalus.ClientApi where

import Prelude
import Daedalus.BackendApi as B
import Control.Monad.Eff (Eff)
import Control.Promise (Promise, fromAff)
import Daedalus.Types (mkCAddress, mkCoin, mkCWalletMeta)
import Data.Argonaut (Json)
import Data.Argonaut.Generic.Aeson (encodeJson)
import Data.Function.Uncurried (Fn4, mkFn4, Fn3, mkFn3)
import Network.HTTP.Affjax (AJAX)

getWallets :: forall eff. Eff(ajax :: AJAX | eff) (Promise Json)
getWallets = fromAff $ map encodeJson B.getWallets

getWallet :: forall eff. String -> (Eff(ajax :: AJAX | eff) (Promise Json))
getWallet = fromAff <<< map encodeJson <<< B.getWallet <<< mkCAddress

getHistory :: forall eff. String -> Eff(ajax :: AJAX | eff) (Promise Json)
getHistory = fromAff <<< map encodeJson <<< B.getHistory <<< mkCAddress

send :: forall eff. Fn3 String String Int (Eff(ajax :: AJAX | eff) (Promise Json))
send = mkFn3 \addrFrom addrTo amount -> fromAff <<< map encodeJson $
    B.send
        (mkCAddress addrFrom)
        (mkCAddress addrTo)
        (mkCoin amount)

newWallet :: forall eff. Fn3 String String String
  (Eff(ajax :: AJAX | eff) (Promise Json))
newWallet = mkFn3 \wType wCurrency wName -> fromAff <<< map encodeJson <<<
    B.newWallet $ mkCWalletMeta wType wCurrency wName

updateWallet :: forall eff. Fn4 String String String String
  (Eff(ajax :: AJAX | eff) (Promise Json))
updateWallet = mkFn4 \addr wType wCurrency wName -> fromAff <<< map encodeJson <<<
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
deleteWallet :: forall eff. String -> (Eff(ajax :: AJAX | eff) (Promise Unit))
deleteWallet = fromAff <<< B.deleteWallet <<< mkCAddress
