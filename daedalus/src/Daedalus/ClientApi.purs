module Daedalus.ClientApi where

import Prelude
import Daedalus.BackendApi as B
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (newRef, REF)
import Control.Promise (Promise, fromAff)
import Daedalus.Types (getProfileLocale, mkCAddress, mkCCoin, mkCWalletMeta, mkCTxId, mkCTxMeta, mkCCurrency, mkCProfile, mkCWalletInit, mkCWalletRedeem, mkCWalletInitIgnoreChecksum, mkBackupPhrase, mkCInitialized, mkCPaperVendWalletRedeem, mkCPassPhrase)
import Daedalus.WS (WSConnection(WSNotConnected), mkWSState, ErrorCb, NotifyCb, openConn)
import Data.Argonaut (Json)
import Data.Argonaut.Generic.Aeson (encodeJson)
import Data.String.Base64 as B64
import Data.Base58 as B58
import Data.Array as A
import Data.String (length, stripSuffix, Pattern (..))
import Data.Maybe (isJust, maybe)
import Data.Function.Eff (EffFn1, mkEffFn1, EffFn2, mkEffFn2, EffFn4, mkEffFn4, EffFn5, mkEffFn5, EffFn3, mkEffFn3, EffFn6, mkEffFn6, EffFn7, mkEffFn7, EffFn8, mkEffFn8)
import Network.HTTP.Affjax (AJAX)
import WebSocket (WEBSOCKET)
import Control.Monad.Error.Class (throwError)
import Data.Either (either)
import Daedalus.Crypto as Crypto
import Daedalus.TLS (TLSOptions, FS, initTLS)
import Node.HTTP (HTTP)

-- TLS

tlsInit :: forall eff. EffFn1 (fs :: FS, err :: EXCEPTION | eff) String TLSOptions
tlsInit = mkEffFn1 initTLS

getLocale :: forall eff. EffFn1 (http :: HTTP, err :: EXCEPTION | eff) TLSOptions (Promise Json)
getLocale = mkEffFn1 \tls -> fromAff $ map encodeJson (getProfileLocale <$> B.getProfile tls)

updateLocale :: forall eff. EffFn2 (http :: HTTP, err :: EXCEPTION | eff) TLSOptions String (Promise Json)
updateLocale = mkEffFn2 \tls locale -> fromAff <<< map encodeJson <<< B.updateProfile tls $ mkCProfile locale

getWallets :: forall eff. EffFn1 (http :: HTTP, err :: EXCEPTION | eff) TLSOptions (Promise Json)
getWallets = mkEffFn1 $ fromAff <<< map encodeJson <<< B.getWallets

getWallet :: forall eff. EffFn2 (http :: HTTP, err :: EXCEPTION | eff) TLSOptions String (Promise Json)
getWallet = mkEffFn2 \tls -> fromAff <<< map encodeJson <<< B.getWallet tls <<< mkCAddress

getHistory :: forall eff. EffFn4 (http :: HTTP, err :: EXCEPTION | eff) TLSOptions String Int Int (Promise Json)
getHistory = mkEffFn4 \tls addr skip limit -> fromAff <<< map encodeJson $
    B.getHistory
        tls
        (mkCAddress addr)
        skip
        limit

searchHistory :: forall eff. EffFn5 (http :: HTTP, err :: EXCEPTION | eff) TLSOptions String String Int Int (Promise Json)
searchHistory = mkEffFn5 \tls addr search skip limit -> fromAff <<< map encodeJson $
    B.searchHistory
        tls
        (mkCAddress addr)
        search
        skip
        limit

send :: forall eff. EffFn5 (http :: HTTP, err :: EXCEPTION | eff) TLSOptions String String String String (Promise Json)
send = mkEffFn5 \tls addrFrom addrTo amount spendingPassword -> fromAff <<< map encodeJson $
    B.send
        tls
        (mkCPassPhrase spendingPassword)
        (mkCAddress addrFrom)
        (mkCAddress addrTo)
        (mkCCoin amount)

sendExtended :: forall eff. EffFn8 (http :: HTTP, err :: EXCEPTION | eff) TLSOptions String String String String String String String (Promise Json)
sendExtended = mkEffFn8 \tls addrFrom addrTo amount curr title desc spendingPassword -> fromAff <<< map encodeJson $
    B.sendExtended
        tls
        (mkCPassPhrase spendingPassword)
        (mkCAddress addrFrom)
        (mkCAddress addrTo)
        (mkCCoin amount)
        (mkCCurrency curr)
        title
        desc

generateMnemonic :: forall eff. Eff (crypto :: Crypto.CRYPTO | eff) String
generateMnemonic = Crypto.generateMnemonic

-- | bip39.validateMnemonic and has at least len words
isValidMnemonic :: forall eff. EffFn2 (crypto :: Crypto.CRYPTO | eff) Int String Boolean
isValidMnemonic = mkEffFn2 \len -> pure <<< either (const false) (const true) <<< mkBackupPhrase len

newWallet :: forall eff . EffFn6 (http :: HTTP, err :: EXCEPTION, crypto :: Crypto.CRYPTO | eff) TLSOptions String String String String String
  (Promise Json)
newWallet = mkEffFn6 \tls wType wCurrency wName mnemonic spendingPassword -> fromAff <<< map encodeJson <<<
    either throwError (B.newWallet tls $ mkCPassPhrase spendingPassword) $ mkCWalletInit wType wCurrency wName mnemonic

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
--   (Eff(http :: HTTP, err :: EXCEPTION, crypto :: Crypto.CRYPTO | eff) (Promise Json))
-- newWalletDeprecated = mkFn4 \wType wCurrency wName wConfirmMnemonic -> fromAff $ map encodeJson $ do
--
--     mnemonic <- liftEff Crypto.generateMnemonic
--     -- FIXME: @jens how did we satisfy this with notify? I am having trouble again
--     isConfirmed <- liftEff' $ unsafeInterleaveEff $ runEffFn1 wConfirmMnemonic mnemonic
--     either throwError B.newWallet $ do
--         isConfirmed
--         mkCWalletInit wType wCurrency wName mnemonic

updateWallet :: forall eff. EffFn7 (http :: HTTP, err :: EXCEPTION | eff) TLSOptions String String String String String Int (Promise Json)
updateWallet = mkEffFn7 \tls addr wType wCurrency wName wAssurance wUnit -> fromAff <<< map encodeJson <<<
    B.updateWallet
        tls
        (mkCAddress addr)
        $ mkCWalletMeta wType wCurrency wName wAssurance wUnit

updateTransaction :: forall eff. EffFn7 (http :: HTTP, err :: EXCEPTION | eff) TLSOptions String String String String String Number (Promise Unit)
updateTransaction = mkEffFn7 \tls addr ctxId ctmCurrency ctmTitle ctmDescription ctmDate -> fromAff <<<
    B.updateTransaction
        tls
        (mkCAddress addr)
        (mkCTxId ctxId)
        $ mkCTxMeta ctmCurrency ctmTitle ctmDescription ctmDate

deleteWallet :: forall eff. EffFn2 (http :: HTTP, err :: EXCEPTION | eff) TLSOptions String (Promise Unit)
deleteWallet = mkEffFn2 \tls -> fromAff <<< B.deleteWallet tls <<< mkCAddress

isValidAddress :: forall eff. EffFn3 (http :: HTTP, err :: EXCEPTION | eff) TLSOptions String String (Promise Boolean)
isValidAddress = mkEffFn3 \tls currency -> fromAff <<< B.isValidAddress tls (mkCCurrency currency)

-- FIXME: notify is not behind TLS yet - we will probably have to modify purescript-websocket-simple
notify :: forall eff. EffFn2 (ref :: REF, ws :: WEBSOCKET, err :: EXCEPTION | eff) (NotifyCb eff) (ErrorCb eff) Unit
notify = mkEffFn2 \messageCb errorCb -> do
    -- TODO (akegalj) grab global (mutable) state of  here
    -- instead of creating newRef
    conn <- newRef WSNotConnected
    openConn $ mkWSState conn messageCb errorCb

blockchainSlotDuration :: forall eff. EffFn1 (http :: HTTP, err :: EXCEPTION | eff) TLSOptions (Promise Int)
blockchainSlotDuration = mkEffFn1 $ fromAff <<< B.blockchainSlotDuration

restoreWallet :: forall eff. EffFn6 (http :: HTTP, err :: EXCEPTION | eff) TLSOptions String String String String String (Promise Json)
restoreWallet = mkEffFn6 \tls wType wCurrency wName spendingPassword -> fromAff <<< map encodeJson <<< either throwError (B.restoreWallet tls $ mkCPassPhrase spendingPassword) <<< mkCWalletInit wType wCurrency wName

restoreWalletIgnoreChecksum :: forall eff. EffFn6 (http :: HTTP, err :: EXCEPTION | eff) TLSOptions String String String String String (Promise Json)
restoreWalletIgnoreChecksum = mkEffFn6 \tls wType wCurrency wName spendingPassword -> fromAff <<< map encodeJson <<< either throwError (B.restoreWallet tls $ mkCPassPhrase spendingPassword) <<< mkCWalletInitIgnoreChecksum wType wCurrency wName

nextUpdate :: forall eff. EffFn1 (http :: HTTP, err :: EXCEPTION | eff) TLSOptions (Promise Json)
nextUpdate = mkEffFn1 $ fromAff <<< map encodeJson <<< B.nextUpdate

applyUpdate :: forall eff. EffFn1 (http :: HTTP, err :: EXCEPTION | eff) TLSOptions (Promise Unit)
applyUpdate = mkEffFn1 $ fromAff <<< B.applyUpdate

systemVersion :: forall eff. EffFn1 (http :: HTTP, err :: EXCEPTION | eff) TLSOptions (Promise Json)
systemVersion = mkEffFn1 $ fromAff <<< map encodeJson <<< B.systemVersion

redeemAda :: forall eff. EffFn3 (http :: HTTP, err :: EXCEPTION, crypto :: Crypto.CRYPTO | eff) TLSOptions String String (Promise Json)
redeemAda = mkEffFn3 \tls seed -> fromAff <<< map encodeJson <<< B.redeemAda tls <<< mkCWalletRedeem seed

redeemAdaPaperVend :: forall eff. EffFn4 (http :: HTTP, err :: EXCEPTION, crypto :: Crypto.CRYPTO | eff) TLSOptions String String String (Promise Json)
redeemAdaPaperVend = mkEffFn4 \tls seed mnemonic -> fromAff <<< map encodeJson <<< either throwError (B.redeemAdaPaperVend tls) <<< mkCPaperVendWalletRedeem seed mnemonic

reportInit :: forall eff. EffFn3 (http :: HTTP, err :: EXCEPTION, crypto :: Crypto.CRYPTO | eff) TLSOptions Int Int (Promise Unit)
reportInit = mkEffFn3 \tls total -> fromAff <<< B.reportInit tls <<< mkCInitialized total

importKey :: forall eff. EffFn2 (http :: HTTP, err :: EXCEPTION | eff) TLSOptions String (Promise Json)
importKey = mkEffFn2 \tls -> fromAff <<< map encodeJson <<< B.importKey tls

syncProgress :: forall eff. EffFn1 (http :: HTTP, err :: EXCEPTION | eff) TLSOptions (Promise Json)
syncProgress = mkEffFn1 $ fromAff <<< map encodeJson <<< B.syncProgress

testReset :: forall eff. EffFn1 (http :: HTTP, err :: EXCEPTION | eff) TLSOptions (Promise Unit)
testReset = mkEffFn1 $ fromAff <<< B.testReset

-- Valid redeem code is base64 encoded 32byte data
-- NOTE: this method handles both base64 and base64url base on rfc4648: see more https://github.com/menelaos/purescript-b64/blob/59e2e9189358a4c8e3eef8662ca281906844e783/src/Data/String/Base64.purs#L182
isValidRedemptionKey :: String -> Boolean
isValidRedemptionKey code = either (const false) (const $ endsWithEqual && 44 == length code) $ B64.decode code
  where
    -- Because it is 32byte base64 encoded
    endsWithEqual = isJust $ stripSuffix (Pattern "=") code

-- Valid paper vend key is base58 encoded 32byte data
isValidPaperVendRedemptionKey :: String -> Boolean
isValidPaperVendRedemptionKey code = maybe false ((==) 32 <<< A.length) $ B58.decode code
