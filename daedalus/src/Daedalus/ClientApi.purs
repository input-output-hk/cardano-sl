module Daedalus.ClientApi where

import Prelude
import Daedalus.BackendApi as B
import Control.Monad.Eff (Eff)
import Control.Promise (Promise, fromAff)
import Daedalus.Types (mkCAddress, mkCoin, mkCWalletMeta, CWallet, mkCTxId, mkCTxMeta, mkCCurrency)
import Data.Argonaut (Json)
import Data.Argonaut.Generic.Aeson (encodeJson)
import Data.Function.Uncurried (Fn2, mkFn2, Fn4, mkFn4, Fn3, mkFn3, Fn6, mkFn6)
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
  (Eff (ajax :: AJAX | eff) (Promise Json))
updateWallet = mkFn4 \addr wType wCurrency wName -> fromAff <<< map encodeJson <<<
    B.updateWallet
        (mkCAddress addr)
        $ mkCWalletMeta wType wCurrency wName

updateTransaction :: forall eff. Fn6 String String String String String Number (Eff (ajax :: AJAX | eff) (Promise Unit))
updateTransaction = mkFn6 \addr ctxId ctmCurrency ctmTitle ctmDescription ctmDate -> fromAff <<<
    B.updateTransaction
        (mkCAddress addr)
        (mkCTxId ctxId)
        $ mkCTxMeta ctmCurrency ctmTitle ctmDescription ctmDate

deleteWallet :: forall eff. String -> (Eff(ajax :: AJAX | eff) (Promise Unit))
deleteWallet = fromAff <<< B.deleteWallet <<< mkCAddress

isValidAddress :: forall eff. Fn2 String String (Eff(ajax :: AJAX | eff) (Promise Boolean))
isValidAddress = mkFn2 \currency -> fromAff <<< B.isValidAddress (mkCCurrency currency)

