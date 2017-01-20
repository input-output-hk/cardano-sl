module Daedalus.ClientApi where

import Prelude
import Daedalus.BackendApi as B
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (newRef, REF)
import Control.Promise (Promise, fromAff)
import Daedalus.Types (mkCAddress, mkCoin, mkCWalletMeta, mkCTxId, mkCTxMeta, mkCCurrency, mkCProfile, mkCWalletInit)
import Daedalus.WS (WSConnection(WSNotConnected), mkWSState, ErrorCb, NotifyCb, openConn)
import Data.Argonaut (Json)
import Data.Argonaut.Generic.Aeson (encodeJson)
import Data.Function.Uncurried (Fn2, mkFn2, Fn4, mkFn4, Fn3, mkFn3, Fn6, mkFn6, Fn7, mkFn7)
import Network.HTTP.Affjax (AJAX)
import WebSocket (WEBSOCKET)
import Control.Monad.Error.Class (throwError)
import Data.Either (either)
import Daedalus.Crypto as Crypto

getProfile :: forall eff. Eff(ajax :: AJAX | eff) (Promise Json)
getProfile = fromAff $ map encodeJson B.getProfile

updateProfile :: forall eff. Fn7 String String String String Number String String (Eff(ajax :: AJAX | eff) (Promise Json))
updateProfile = mkFn7 \name email phone pass date locale picture -> fromAff <<< map encodeJson <<< B.updateProfile $ mkCProfile name email phone pass date locale picture

getWallets :: forall eff. Eff(ajax :: AJAX | eff) (Promise Json)
getWallets = fromAff $ map encodeJson B.getWallets

getWallet :: forall eff. String -> (Eff(ajax :: AJAX | eff) (Promise Json))
getWallet = fromAff <<< map encodeJson <<< B.getWallet <<< mkCAddress

getHistory :: forall eff. String -> Eff(ajax :: AJAX | eff) (Promise Json)
getHistory = fromAff <<< map encodeJson <<< B.getHistory <<< mkCAddress

searchHistory :: forall eff. Fn3 String String Int (Eff(ajax :: AJAX | eff) (Promise Json))
searchHistory = mkFn3 \addr search limit -> fromAff <<< map encodeJson $
    B.searchHistory
        (mkCAddress addr)
        search
        limit

send :: forall eff. Fn3 String String Int (Eff(ajax :: AJAX | eff) (Promise Json))
send = mkFn3 \addrFrom addrTo amount -> fromAff <<< map encodeJson $
    B.send
        (mkCAddress addrFrom)
        (mkCAddress addrTo)
        (mkCoin amount)

sendExtended :: forall eff. Fn6 String String Int String String String (Eff(ajax :: AJAX | eff) (Promise Json))
sendExtended = mkFn6 \addrFrom addrTo amount curr title desc -> fromAff <<< map encodeJson $
    B.sendExtended
        (mkCAddress addrFrom)
        (mkCAddress addrTo)
        (mkCoin amount)
        (mkCCurrency curr)
        title
        desc

generateMnemonic :: forall eff. Eff (crypto :: Crypto.CRYPTO | eff) String
generateMnemonic = Crypto.generateMnemonic

newWallet :: forall eff. Fn4 String String String String
  (Eff(ajax :: AJAX | eff) (Promise Json))
newWallet = mkFn4 \wType wCurrency wName wBackupPhrase -> fromAff <<< map encodeJson <<<
    either throwError B.newWallet $ mkCWalletInit wType wCurrency wName wBackupPhrase

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

notify :: forall eff. Fn2 NotifyCb ErrorCb (Eff (ref :: REF, ws :: WEBSOCKET, err :: EXCEPTION | eff) Unit)
notify = mkFn2 \messageCb errorCb -> do
    -- TODO (akegalj) grab global (mutable) state of  here
    -- instead of creating newRef
    conn <- newRef WSNotConnected
    openConn $ mkWSState conn messageCb errorCb
