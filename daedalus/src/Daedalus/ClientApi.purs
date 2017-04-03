module Daedalus.ClientApi where

import Prelude
import Daedalus.BackendApi as B
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (newRef, REF)
import Control.Promise (Promise, fromAff)
import Daedalus.Types (getProfileLocale, mkCAddress, mkCoin, mkCWalletMeta, mkCTxId, mkCTxMeta, mkCCurrency, mkCProfile, mkCWalletInit, mkCWalletRedeem, mkCWalletInitIgnoreChecksum, mkBackupPhrase, mkCInitialized, mkCPassPhrase, mkCPostVendWalletRedeem)
import Daedalus.WS (WSConnection(WSNotConnected), mkWSState, ErrorCb, NotifyCb, openConn)
import Data.Argonaut (Json)
import Data.Argonaut.Generic.Aeson (encodeJson)
import Data.String.Base64 as B64
import Data.Base58 as B58
import Data.String (length, stripSuffix, Pattern (..))
import Data.Maybe (isJust, maybe)
import Data.Function.Eff (EffFn1, mkEffFn1, EffFn2, mkEffFn2, EffFn4, mkEffFn4, EffFn3, mkEffFn3, EffFn6, mkEffFn6, EffFn7, mkEffFn7, mkEffFn5, EffFn5)
import Network.HTTP.Affjax (AJAX)
import WebSocket (WEBSOCKET)
import Control.Monad.Error.Class (throwError)
import Data.Either (either)
import Daedalus.Crypto as Crypto

getLocale :: forall eff. Eff (ajax :: AJAX | eff) (Promise Json)
getLocale = fromAff $ map encodeJson (getProfileLocale <$> B.getProfile)

updateLocale :: forall eff. EffFn1 (ajax :: AJAX | eff) String (Promise Json)
updateLocale = mkEffFn1 \locale -> fromAff <<< map encodeJson <<< B.updateProfile $ mkCProfile locale

getWallets :: forall eff. Eff (ajax :: AJAX | eff) (Promise Json)
getWallets = fromAff $ map encodeJson B.getWallets

getWallet :: forall eff. EffFn1 (ajax :: AJAX | eff) String (Promise Json)
getWallet = mkEffFn1 $ fromAff <<< map encodeJson <<< B.getWallet <<< mkCAddress

getHistory :: forall eff. EffFn3 (ajax :: AJAX | eff) String Int Int (Promise Json)
getHistory = mkEffFn3 \addr skip limit -> fromAff <<< map encodeJson $
    B.getHistory
        (mkCAddress addr)
        skip
        limit

searchHistory :: forall eff. EffFn4 (ajax :: AJAX | eff) String String Int Int (Promise Json)
searchHistory = mkEffFn4 \addr search skip limit -> fromAff <<< map encodeJson $
    B.searchHistory
        (mkCAddress addr)
        search
        skip
        limit

send :: forall eff. EffFn4 (ajax :: AJAX | eff) String String String Int (Promise Json)
send = mkEffFn4 \pass addrFrom addrTo amount -> fromAff <<< map encodeJson $
    B.send
        (mkCPassPhrase pass)
        (mkCAddress addrFrom)
        (mkCAddress addrTo)
        (mkCoin amount)

sendExtended :: forall eff. EffFn7 (ajax :: AJAX | eff) String String String Int String String String (Promise Json)
sendExtended = mkEffFn7 \pass addrFrom addrTo amount curr title desc -> fromAff <<< map encodeJson $
    B.sendExtended
        (mkCPassPhrase pass)
        (mkCAddress addrFrom)
        (mkCAddress addrTo)
        (mkCoin amount)
        (mkCCurrency curr)
        title
        desc

generateMnemonic :: forall eff. Eff (crypto :: Crypto.CRYPTO | eff) String
generateMnemonic = Crypto.generateMnemonic

-- | bip39.validateMnemonic and has at least 12 words
isValidMnemonic :: forall eff. EffFn1 (crypto :: Crypto.CRYPTO | eff) String Boolean
isValidMnemonic = mkEffFn1 $ pure <<< either (const false) (const true) <<< mkBackupPhrase

newWallet :: forall eff . EffFn5 (ajax :: AJAX, crypto :: Crypto.CRYPTO | eff) String String String String String
  (Promise Json)
newWallet = mkEffFn5 \pass wType wCurrency wName mnemonic -> fromAff <<< map encodeJson <<<
    either throwError (B.newWallet $ mkCPassPhrase pass) $ mkCWalletInit wType wCurrency wName mnemonic

-- NOTE: https://issues.serokell.io/issue/DAE-33#comment=96-1798
-- Daedalus.ClientApi.newWallet(
--     'CWTPersonal'
--   , 'ADA'
--   , 'wallet name'
--   , function(mnemonics) {
--     // if this function finishes we will send request for wallet
--     // creation to the backend. That means user validated and
--     // stored mnemonics.
--     // if an exception is thrown new wallet request will be aborted
--     // and promise should return thrown error
--     if(userSavedMnemonics) {
--       // do nothing
--     } else {
--       throw new Error("Wallet canceled")
--     }
--   }
--   )()
--   .then(function(value) {
--     console.log('SUCCESS', value);
--   }, function(reason) {
--     console.log('ERROR', reason);
--   })
-- newWalletDeprecated :: forall eff . Fn4 String String String (EffFn1 (err :: EXCEPTION | eff) String Unit)
--   (Eff(ajax :: AJAX, crypto :: Crypto.CRYPTO | eff) (Promise Json))
-- newWalletDeprecated = mkFn4 \wType wCurrency wName wConfirmMnemonic -> fromAff $ map encodeJson $ do
--
--     mnemonic <- liftEff Crypto.generateMnemonic
--     -- FIXME: @jens how did we satisfy this with notify? I am having trouble again
--     isConfirmed <- liftEff' $ unsafeInterleaveEff $ runEffFn1 wConfirmMnemonic mnemonic
--     either throwError B.newWallet $ do
--         isConfirmed
--         mkCWalletInit wType wCurrency wName mnemonic

updateWallet :: forall eff. EffFn6 (ajax :: AJAX | eff) String String String String String Int (Promise Json)
updateWallet = mkEffFn6 \addr wType wCurrency wName wAssurance wUnit -> fromAff <<< map encodeJson <<<
    B.updateWallet
        (mkCAddress addr)
        $ mkCWalletMeta wType wCurrency wName wAssurance wUnit

updateTransaction :: forall eff. EffFn6 (ajax :: AJAX | eff) String String String String String Number (Promise Unit)
updateTransaction = mkEffFn6 \addr ctxId ctmCurrency ctmTitle ctmDescription ctmDate -> fromAff <<<
    B.updateTransaction
        (mkCAddress addr)
        (mkCTxId ctxId)
        $ mkCTxMeta ctmCurrency ctmTitle ctmDescription ctmDate

deleteWallet :: forall eff. EffFn1 (ajax :: AJAX | eff) String (Promise Unit)
deleteWallet = mkEffFn1 $ fromAff <<< B.deleteWallet <<< mkCAddress

isValidAddress :: forall eff. EffFn2 (ajax :: AJAX | eff) String String (Promise Boolean)
isValidAddress = mkEffFn2 \currency -> fromAff <<< B.isValidAddress (mkCCurrency currency)

notify :: forall eff. EffFn2 (ref :: REF, ws :: WEBSOCKET, err :: EXCEPTION | eff) (NotifyCb eff) (ErrorCb eff) Unit
notify = mkEffFn2 \messageCb errorCb -> do
    -- TODO (akegalj) grab global (mutable) state of  here
    -- instead of creating newRef
    conn <- newRef WSNotConnected
    openConn $ mkWSState conn messageCb errorCb

blockchainSlotDuration :: forall eff. Eff (ajax :: AJAX | eff) (Promise Int)
blockchainSlotDuration = fromAff B.blockchainSlotDuration

restoreWallet :: forall eff. EffFn5 (ajax :: AJAX | eff) String String String String String (Promise Json)
restoreWallet = mkEffFn5 \pass wType wCurrency wName -> fromAff <<< map encodeJson <<< either throwError (B.restoreWallet $ mkCPassPhrase pass) <<< mkCWalletInit wType wCurrency wName

restoreWalletIgnoreChecksum :: forall eff. EffFn5 (ajax :: AJAX | eff) String String String String String (Promise Json)
restoreWalletIgnoreChecksum = mkEffFn5 \pass wType wCurrency wName -> fromAff <<< map encodeJson <<< either throwError (B.restoreWallet $ mkCPassPhrase pass) <<< mkCWalletInitIgnoreChecksum wType wCurrency wName

nextUpdate :: forall eff. Eff (ajax :: AJAX | eff) (Promise Json)
nextUpdate = fromAff $ map encodeJson B.nextUpdate

applyUpdate :: forall eff. Eff (ajax :: AJAX | eff) (Promise Unit)
applyUpdate = fromAff B.applyUpdate

systemVersion :: forall eff. Eff (ajax :: AJAX | eff) (Promise Json)
systemVersion = fromAff $ map encodeJson B.systemVersion

redeemADA :: forall eff. EffFn2 (ajax :: AJAX, crypto :: Crypto.CRYPTO | eff) String String (Promise Json)
redeemADA = mkEffFn2 \seed -> fromAff <<< map encodeJson <<< B.redeemADA <<< mkCWalletRedeem seed

postVendRedeemADA :: forall eff. EffFn3 (ajax :: AJAX, crypto :: Crypto.CRYPTO | eff) String String String (Promise Json)
postVendRedeemADA = mkEffFn3 \seed mnemonic -> fromAff <<< map encodeJson <<< either throwError B.postVendRedeemADA <<< mkCPostVendWalletRedeem seed mnemonic

reportInit :: forall eff. EffFn2 (ajax :: AJAX, crypto :: Crypto.CRYPTO | eff) Int Int (Promise Unit)
reportInit = mkEffFn2 \total -> fromAff <<< B.reportInit <<< mkCInitialized total

importKey :: forall eff. EffFn1 (ajax :: AJAX | eff) String (Promise Json)
importKey = mkEffFn1 $ fromAff <<< map encodeJson <<< B.importKey

syncProgress :: forall eff. Eff (ajax :: AJAX | eff) (Promise Json)
syncProgress = fromAff $ map encodeJson B.syncProgress

testReset :: forall eff. Eff (ajax :: AJAX | eff) (Promise Unit)
testReset = fromAff B.testReset

-- Valid redeem code is base64 encoded 32byte data
-- NOTE: this method handles both base64 and base64url base on rfc4648: see more https://github.com/menelaos/purescript-b64/blob/59e2e9189358a4c8e3eef8662ca281906844e783/src/Data/String/Base64.purs#L182
isValidRedeemCode :: String -> Boolean
isValidRedeemCode code = either (const false) (const $ endsWithEqual && 44 == length code) $ B64.decode code
  where
    -- Because it is 32byte base64 encoded
    endsWithEqual = isJust $ stripSuffix (Pattern "=") code

-- Valid postvend code is base58 encoded 32byte data
isValidPostVendRedeemCode :: String -> Boolean
isValidPostVendRedeemCode code = maybe false (const $ 44 == length code) $ B58.decode code
