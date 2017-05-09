module Daedalus.ClientApi where

import Prelude
import Daedalus.BackendApi as B
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (newRef, REF)
import Control.Promise (Promise, fromAff)
import Daedalus.Types (getProfileLocale, mkCAddress, mkCCoin, mkCWalletMeta, mkCTxId, mkCTxMeta, mkCCurrency, mkCProfile, mkCWalletInit, mkCWalletRedeem, mkBackupPhrase, mkCInitialized, mkCPaperVendWalletRedeem, mkCPassPhrase, mkCWalletSetInit)
import Daedalus.WS (WSConnection(WSNotConnected), mkWSState, ErrorCb, NotifyCb, openConn)
import Data.Argonaut (Json)
import Data.Argonaut.Generic.Aeson (encodeJson, decodeJson)
import Data.String.Base64 as B64
import Data.Base58 as B58
import Data.Array as A
import Data.String (length, stripSuffix, Pattern (..))
import Data.Maybe (isJust, maybe, Maybe (..))
import Data.Function.Eff (EffFn1, mkEffFn1, EffFn2, mkEffFn2, EffFn4, mkEffFn4, EffFn5, mkEffFn5, EffFn3, mkEffFn3, EffFn6, mkEffFn6, EffFn7, mkEffFn7)
import Network.HTTP.Affjax (AJAX)
import WebSocket (WEBSOCKET)
import Control.Monad.Error.Class (throwError)
import Data.Either (either)
import Daedalus.Crypto as Crypto

--------------------------------------------------------------------------------
-- TEST ------------------------------------------------------------------------

-- | Resets wallet database (metadata) and wallets/keys
-- This should be used only in testing
-- Arguments:
-- Returns:
-- Example in nodejs:
testReset :: forall eff. Eff (ajax :: AJAX | eff) (Promise Unit)
testReset = fromAff B.testReset

--------------------------------------------------------------------------------
-- Wallet Sets ---------------------------------------------------------------------

-- | Gets specified wallet set
-- Arguments: wallet set id/hash
-- Returns json representation of requested wallet set
-- Example in nodejs:
--
getWalletSet :: forall eff. EffFn1 (ajax :: AJAX | eff) String (Promise Json)
getWalletSet = mkEffFn1 $ fromAff <<< map encodeJson <<< B.getWalletSet <<< mkCAddress

-- | Gets all wallet sets
-- Arguments:
-- Returns json representation of all wallet sets
-- Example in nodejs:
--
getWalletSets :: forall eff. Eff (ajax :: AJAX | eff) (Promise Json)
getWalletSets = fromAff $ map encodeJson B.getWalletSets

-- | Creates a new wallet set.
-- Arguments: wallet set name, mnemonics, spending password (set to empty string if you don't want to set password)
-- Returns json representation of created wallet set
-- Example in nodejs:
--
newWalletSet :: forall eff . EffFn3 (ajax :: AJAX, crypto :: Crypto.CRYPTO | eff) String String String
  (Promise Json)
newWalletSet = mkEffFn3 \wSetName mnemonic spendingPassword -> fromAff <<< map encodeJson <<<
    either throwError (B.newWalletSet $ mkCPassPhrase spendingPassword) $ mkCWalletSetInit wSetName mnemonic

-- TODO: note that restoreWalletSet and newWalletSet are the same. They will be unified in future

-- | Restores a new wallet set.
-- Arguments: wallet set name, mnemonics, spending password (set to empty string if you don't want to set password
-- Returns json representation of restored wallet set
-- Example in nodejs:
--
restoreWalletSet :: forall eff. EffFn3 (ajax :: AJAX | eff) String String String (Promise Json)
restoreWalletSet = mkEffFn3 \wSetName mnemonic spendingPassword -> fromAff <<< map encodeJson <<< either throwError (B.restoreWalletSet $ mkCPassPhrase spendingPassword) $ mkCWalletSetInit wSetName mnemonic

-- | Rename a wallet set.
-- Arguments: wallet set id/hash, name
-- Returns json representation of renamed wallet set
-- Example in nodejs:
--
renameWalletSet :: forall eff. EffFn2 (ajax :: AJAX | eff) String String (Promise Json)
renameWalletSet = mkEffFn2 \wSetId name -> fromAff <<< map encodeJson $ B.renameWalletSet (mkCAddress wSetId) name

-- | Import a wallet set.
-- Arguments: file path to the wallet set on a filesystem, spending password (set to empty string if you don't want to set password)
-- Returns json representation of imported wallet set
-- Example in nodejs:
--
importWalletSet :: forall eff. EffFn2 (ajax :: AJAX | eff) String String (Promise Json)
importWalletSet = mkEffFn2 \filePath spendingPassword -> fromAff <<< map encodeJson $ B.importWalletSet (mkCPassPhrase spendingPassword) filePath

-- | Rename a wallet set.
-- Arguments: wallet set id/hash, old spending password (set to empty string if there is no password), new spending password (set to empty string if you want to remove password)
-- Returns json representation of renamed wallet set
-- Example in nodejs:
--
changeWalletSetPass :: forall eff. EffFn3 (ajax :: AJAX | eff) String String String (Promise Json)
changeWalletSetPass = mkEffFn3 \wSetId oldPass newPass -> fromAff <<< map encodeJson $ B.changeWalletSetPass (mkCAddress wSetId) (mkCPassPhrase oldPass) (mkCPassPhrase newPass)


--------------------------------------------------------------------------------
-- Wallets ---------------------------------------------------------------------

-- | Get a wallet.
-- Arguments: wallet object/identifier
-- Returns json representation of a wallet
-- Example in nodejs:
--
getWallet :: forall eff. EffFn1 (ajax :: AJAX | eff) Json (Promise Json)
getWallet = mkEffFn1 $ fromAff <<< map encodeJson <<< either (throwError <<< error) B.getWallet <<< decodeJson

-- | Get all wallets.
-- Arguments:
-- Returns json representation of all wallet sets
-- Example in nodejs:
--
getWallets :: forall eff. Eff (ajax :: AJAX | eff) (Promise Json)
getWallets = fromAff $ map encodeJson $ B.getWallets Nothing

-- | Get wallets from specific wallet set.
-- Arguments: address/hash/id of a wallet set
-- Returns json representation of wallets within given wallet set id
-- Example in nodejs:
--
getSetWallets :: forall eff. EffFn1 (ajax :: AJAX | eff) String (Promise Json)
getSetWallets = mkEffFn1 $ fromAff <<< map encodeJson <<< B.getWallets <<< Just <<< mkCAddress

-- | Get meta information from given wallet
-- Arguments: wallet object/identifier, type, currency, name, assurance, unit
-- Returns json representation of wallets within given wallet id
-- Example in nodejs:
--
updateWallet :: forall eff. EffFn6 (ajax :: AJAX | eff) Json String String String String Int (Promise Json)
updateWallet = mkEffFn6 \wId wType wCurrency wName wAssurance wUnit -> fromAff <<< map encodeJson <<<
    either (throwError <<< error)
        (flip B.updateWallet $ mkCWalletMeta wType wCurrency wName wAssurance wUnit)
        $ decodeJson wId

-- | Creates a new wallet.
-- Arguments: address/hash/id of a wallet set, type, currency, name, mnemonics, spending password (if empty string is given, wallet will be created with no spending password)
-- Returns json representation of newly created wallet
-- Example in nodejs:
--
newWallet :: forall eff . EffFn6 (ajax :: AJAX | eff) String String String String String String
  (Promise Json)
newWallet = mkEffFn6 \wSetId wType wCurrency wName mnemonic spendingPassword -> fromAff <<< map encodeJson <<<
    B.newWallet (mkCPassPhrase spendingPassword) $ mkCWalletInit wType wCurrency wName (mkCAddress wSetId)

-- | Deletes a wallet.
-- Arguments: wallet object/identifier
-- Returns:
-- Example in nodejs:
--
deleteWallet :: forall eff. EffFn1 (ajax :: AJAX | eff) Json (Promise Unit)
deleteWallet = mkEffFn1 $ fromAff <<< either (throwError <<< error) B.deleteWallet <<< decodeJson

--------------------------------------------------------------------------------
-- Accounts ------------------------------------------------------------------

-- | Creates a new wallet.
-- Arguments: wallet set, type, currency, name, mnemonics, spending password (if empty string is given, wallet will be created with no spending password)
-- Returns json representation of newly created wallet
-- Example in nodejs:
--
newAccount :: forall eff . EffFn2 (ajax :: AJAX | eff) Json String
  (Promise Json)
newAccount = mkEffFn2 \wId spendingPassword -> fromAff <<< map encodeJson <<<
    either (throwError <<< error) (B.newAccount $ mkCPassPhrase spendingPassword) $ decodeJson wId

--------------------------------------------------------------------------------
-- Addresses ------------------------------------------------------------------

-- | Checks is some string correct representation of a valid address
-- Arguments: currency for which we are checking address, string representation of an address to check
-- Returns true if address is valid in specific currency or false otherwise
-- Example in nodejs:
--
isValidAddress :: forall eff. EffFn2 (ajax :: AJAX | eff) String String (Promise Boolean)
isValidAddress = mkEffFn2 \currency -> fromAff <<< B.isValidAddress (mkCCurrency currency)


--------------------------------------------------------------------------------
-- Profiles --------------------------------------------------------------------

getLocale :: forall eff. Eff (ajax :: AJAX | eff) (Promise Json)
getLocale = fromAff $ map encodeJson (getProfileLocale <$> B.getProfile)

updateLocale :: forall eff. EffFn1 (ajax :: AJAX | eff) String (Promise Json)
updateLocale = mkEffFn1 \locale -> fromAff <<< map encodeJson <<< B.updateProfile $ mkCProfile locale

--------------------------------------------------------------------------------
-- Transactions ----------------------------------------------------------------

-- getWallets :: forall eff. Eff (ajax :: AJAX | eff) (Promise Json)
-- getWallets = fromAff $ map encodeJson B.getWallets
--
-- getWallet :: forall eff. EffFn1 (ajax :: AJAX | eff) String (Promise Json)
-- getWallet = mkEffFn1 $ fromAff <<< map encodeJson <<< B.getWallet <<< mkCAddress
--
-- getHistory :: forall eff. EffFn3 (ajax :: AJAX | eff) String Int Int (Promise Json)
-- getHistory = mkEffFn3 \addr skip limit -> fromAff <<< map encodeJson $
--     B.getHistory
--         (mkCAddress addr)
--         skip
--         limit
--
-- searchHistory :: forall eff. EffFn4 (ajax :: AJAX | eff) String String Int Int (Promise Json)
-- searchHistory = mkEffFn4 \addr search skip limit -> fromAff <<< map encodeJson $
--     B.searchHistory
--         (mkCAddress addr)
--         search
--         skip
--         limit
--
-- send :: forall eff. EffFn4 (ajax :: AJAX | eff) String String String String (Promise Json)
-- send = mkEffFn4 \addrFrom addrTo amount spendingPassword -> fromAff <<< map encodeJson $
--     B.send
--         (mkCPassPhrase spendingPassword)
--         (mkCAddress addrFrom)
--         (mkCAddress addrTo)
--         (mkCCoin amount)
--
-- sendExtended :: forall eff. EffFn7 (ajax :: AJAX | eff) String String String String String String String (Promise Json)
-- sendExtended = mkEffFn7 \addrFrom addrTo amount curr title desc spendingPassword -> fromAff <<< map encodeJson $
--     B.sendExtended
--         (mkCPassPhrase spendingPassword)
--         (mkCAddress addrFrom)
--         (mkCAddress addrTo)
--         (mkCCoin amount)
--         (mkCCurrency curr)
--         title
--         desc
--
-- generateMnemonic :: forall eff. Eff (crypto :: Crypto.CRYPTO | eff) String
-- generateMnemonic = Crypto.generateMnemonic
--
-- -- | bip39.validateMnemonic and has at least len words
-- isValidMnemonic :: forall eff. EffFn2 (crypto :: Crypto.CRYPTO | eff) Int String Boolean
-- isValidMnemonic = mkEffFn2 \len -> pure <<< either (const false) (const true) <<< mkBackupPhrase len
--
-- newWallet :: forall eff . EffFn5 (ajax :: AJAX, crypto :: Crypto.CRYPTO | eff) String String String String String
--   (Promise Json)
-- newWallet = mkEffFn5 \wType wCurrency wName mnemonic spendingPassword -> fromAff <<< map encodeJson <<<
--     either throwError (B.newWallet $ mkCPassPhrase spendingPassword) $ mkCWalletInit wType wCurrency wName mnemonic
--
-- -- NOTE: https://issues.serokell.io/issue/DAE-33#comment=96-1798
-- -- Daedalus.ClientApi.newWallet(
-- --     'CWTPersonal'
-- --   , 'ADA'
-- --   , 'wallet name'
-- --   , function(mnemonics) {
-- --     // if this function finishes we will send request for wallet
-- --     // creation to the backend. That means user validated and
-- --     // stored mnemonics.
-- --     // if an exception is thrown new wallet request will be aborted
-- --     // and promise should return thrown error
-- --     if(userSavedMnemonics) {
-- --       // do nothing
-- --     } else {
-- --       throw new Error("Wallet canceled")
-- --     }
-- --   }
-- --   )()
-- --   .then(function(value) {
-- --     console.log('SUCCESS', value);
-- --   }, function(reason) {
-- --     console.log('ERROR', reason);
-- --   })
-- -- newWalletDeprecated :: forall eff . Fn4 String String String (EffFn1 (err :: EXCEPTION | eff) String Unit)
-- --   (Eff(ajax :: AJAX, crypto :: Crypto.CRYPTO | eff) (Promise Json))
-- -- newWalletDeprecated = mkFn4 \wType wCurrency wName wConfirmMnemonic -> fromAff $ map encodeJson $ do
-- --
-- --     mnemonic <- liftEff Crypto.generateMnemonic
-- --     -- FIXME: @jens how did we satisfy this with notify? I am having trouble again
-- --     isConfirmed <- liftEff' $ unsafeInterleaveEff $ runEffFn1 wConfirmMnemonic mnemonic
-- --     either throwError B.newWallet $ do
-- --         isConfirmed
-- --         mkCWalletInit wType wCurrency wName mnemonic
--
-- updateWallet :: forall eff. EffFn6 (ajax :: AJAX | eff) String String String String String Int (Promise Json)
-- updateWallet = mkEffFn6 \addr wType wCurrency wName wAssurance wUnit -> fromAff <<< map encodeJson <<<
--     B.updateWallet
--         (mkCAddress addr)
--         $ mkCWalletMeta wType wCurrency wName wAssurance wUnit
--
-- updateTransaction :: forall eff. EffFn6 (ajax :: AJAX | eff) String String String String String Number (Promise Unit)
-- updateTransaction = mkEffFn6 \addr ctxId ctmCurrency ctmTitle ctmDescription ctmDate -> fromAff <<<
--     B.updateTransaction
--         (mkCAddress addr)
--         (mkCTxId ctxId)
--         $ mkCTxMeta ctmCurrency ctmTitle ctmDescription ctmDate
--
-- deleteWallet :: forall eff. EffFn1 (ajax :: AJAX | eff) String (Promise Unit)
-- deleteWallet = mkEffFn1 $ fromAff <<< B.deleteWallet <<< mkCAddress
--
-- isValidAddress :: forall eff. EffFn2 (ajax :: AJAX | eff) String String (Promise Boolean)
-- isValidAddress = mkEffFn2 \currency -> fromAff <<< B.isValidAddress (mkCCurrency currency)
--
-- notify :: forall eff. EffFn2 (ref :: REF, ws :: WEBSOCKET, err :: EXCEPTION | eff) (NotifyCb eff) (ErrorCb eff) Unit
-- notify = mkEffFn2 \messageCb errorCb -> do
--     -- TODO (akegalj) grab global (mutable) state of  here
--     -- instead of creating newRef
--     conn <- newRef WSNotConnected
--     openConn $ mkWSState conn messageCb errorCb
--
-- blockchainSlotDuration :: forall eff. Eff (ajax :: AJAX | eff) (Promise Int)
-- blockchainSlotDuration = fromAff B.blockchainSlotDuration
--
-- restoreWallet :: forall eff. EffFn5 (ajax :: AJAX | eff) String String String String String (Promise Json)
-- restoreWallet = mkEffFn5 \wType wCurrency wName spendingPassword -> fromAff <<< map encodeJson <<< either throwError (B.restoreWallet $ mkCPassPhrase spendingPassword) <<< mkCWalletInit wType wCurrency wName
--
--
-- nextUpdate :: forall eff. Eff (ajax :: AJAX | eff) (Promise Json)
-- nextUpdate = fromAff $ map encodeJson B.nextUpdate
--
-- applyUpdate :: forall eff. Eff (ajax :: AJAX | eff) (Promise Unit)
-- applyUpdate = fromAff B.applyUpdate
--
-- systemVersion :: forall eff. Eff (ajax :: AJAX | eff) (Promise Json)
-- systemVersion = fromAff $ map encodeJson B.systemVersion
--
-- redeemAda :: forall eff. EffFn2 (ajax :: AJAX, crypto :: Crypto.CRYPTO | eff) String String (Promise Json)
-- redeemAda = mkEffFn2 \seed -> fromAff <<< map encodeJson <<< B.redeemAda <<< mkCWalletRedeem seed
--
-- redeemAdaPaperVend :: forall eff. EffFn3 (ajax :: AJAX, crypto :: Crypto.CRYPTO | eff) String String String (Promise Json)
-- redeemAdaPaperVend = mkEffFn3 \seed mnemonic -> fromAff <<< map encodeJson <<< either throwError B.redeemAdaPaperVend <<< mkCPaperVendWalletRedeem seed mnemonic
--
-- reportInit :: forall eff. EffFn2 (ajax :: AJAX, crypto :: Crypto.CRYPTO | eff) Int Int (Promise Unit)
-- reportInit = mkEffFn2 \total -> fromAff <<< B.reportInit <<< mkCInitialized total
--
-- importKey :: forall eff. EffFn1 (ajax :: AJAX | eff) String (Promise Json)
-- importKey = mkEffFn1 $ fromAff <<< map encodeJson <<< B.importKey
--
-- syncProgress :: forall eff. Eff (ajax :: AJAX | eff) (Promise Json)
-- syncProgress = fromAff $ map encodeJson B.syncProgress
--
-- -- Valid redeem code is base64 encoded 32byte data
-- -- NOTE: this method handles both base64 and base64url base on rfc4648: see more https://github.com/menelaos/purescript-b64/blob/59e2e9189358a4c8e3eef8662ca281906844e783/src/Data/String/Base64.purs#L182
-- isValidRedemptionKey :: String -> Boolean
-- isValidRedemptionKey code = either (const false) (const $ endsWithEqual && 44 == length code) $ B64.decode code
--   where
--     -- Because it is 32byte base64 encoded
--     endsWithEqual = isJust $ stripSuffix (Pattern "=") code
--
-- -- Valid paper vend key is base58 encoded 32byte data
-- isValidPaperVendRedemptionKey :: String -> Boolean
-- isValidPaperVendRedemptionKey code = maybe false ((==) 32 <<< A.length) $ B58.decode code
