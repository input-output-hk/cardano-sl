module Daedalus.ClientApi where

import Prelude
import Daedalus.BackendApi as B
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, error)
import Control.Monad.Eff.Ref (newRef, REF)
import Control.Promise (Promise, fromAff)
import Daedalus.Types (getProfileLocale, mkCAddress, mkCCoin, mkCWalletMeta, mkCTxId, mkCTxMeta, mkCCurrency, mkCProfile, mkCWalletInit, mkCWalletRedeem, mkBackupPhrase, mkCInitialized, mkCPaperVendWalletRedeem, mkCPassPhrase, mkCWalletSetInit)
import Daedalus.WS (WSConnection(WSNotConnected), mkWSState, ErrorCb, NotifyCb, openConn)
import Data.Bifunctor (lmap)
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
-- | This should be used only in testing.
-- |
-- | Example in nodejs:
-- | ```js
-- | > api.testReset().then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > {}
-- | ```
testReset :: forall eff. Eff (ajax :: AJAX | eff) (Promise Unit)
testReset = fromAff B.testReset

--------------------------------------------------------------------------------
-- Wallet Sets ---------------------------------------------------------------------

-- | Gets specified wallet set
-- Arguments: wallet set id/hash
-- Returns json representation of requested wallet set
-- Example in nodejs:
-- | ```js
-- | > api.getWalletSet('1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > { cwsWalletsNumber: 0,
-- |   cwsWSetMeta: { cwsName: 'test' },
-- |   cwsPassphraseLU: 1494583348.3572557,
-- |   cwsHasPassphrase: true,
-- |   cwsAddress: '1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW' }
-- | ```
getWalletSet :: forall eff. EffFn1 (ajax :: AJAX | eff) String (Promise Json)
getWalletSet = mkEffFn1 $ fromAff <<< map encodeJson <<< B.getWalletSet <<< mkCAddress

-- | Gets all wallet sets
-- Arguments:
-- Returns json representation of all wallet sets
-- Example in nodejs:
-- | ```js
-- | > api.getWalletSets().then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > [ { cwsWalletsNumber: 0,
-- |     cwsWSetMeta: { cwsName: 'test' },
-- |     cwsPassphraseLU: 1494583348.3572557,
-- |     cwsHasPassphrase: true,
-- |     cwsAddress: '1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW' } ]
-- | ```
getWalletSets :: forall eff. Eff (ajax :: AJAX | eff) (Promise Json)
getWalletSets = fromAff $ map encodeJson B.getWalletSets

-- | Creates a new wallet set.
-- Arguments: wallet set name, mnemonics, spending password (set to empty string if you don't want to set password)
-- Returns json representation of created wallet set
-- Example in nodejs:
-- | ```js
-- | > api.newWalletSet('test', 'transfer uniform grunt excess six veteran vintage warm confirm vote nephew allow', 'pass').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > { cwsWalletsNumber: 0,
-- |   cwsWSetMeta: { cwsName: 'test' },
-- |   cwsPassphraseLU: 1494583348.3572557,
-- |   cwsHasPassphrase: true,
-- |   cwsAddress: '1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW' }
-- | ```
newWalletSet :: forall eff . EffFn3 (ajax :: AJAX, crypto :: Crypto.CRYPTO | eff) String String String
  (Promise Json)
newWalletSet = mkEffFn3 \wSetName mnemonic spendingPassword -> fromAff <<< map encodeJson <<<
    either throwError (B.newWalletSet $ mkCPassPhrase spendingPassword) $ mkCWalletSetInit wSetName mnemonic

-- TODO: note that restoreWalletSet and newWalletSet are the same. They will be unified in future

-- | Restores a new wallet set.
-- Arguments: wallet set name, mnemonics, spending password (set to empty string if you don't want to set password
-- Returns json representation of restored wallet set
-- Example in nodejs:
-- | > api.restoreWalletSet('test', 'transfer uniform grunt excess six veteran vintage warm confirm vote nephew allow', 'pass').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > { cwsWalletsNumber: 0,
-- |   cwsWSetMeta: { cwsName: 'test' },
-- |   cwsPassphraseLU: 1494846878.0783634,
-- |   cwsHasPassphrase: true,
-- |   cwsAddress: '1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW' }
restoreWalletSet :: forall eff. EffFn3 (ajax :: AJAX | eff) String String String (Promise Json)
restoreWalletSet = mkEffFn3 \wSetName mnemonic spendingPassword -> fromAff <<< map encodeJson <<< either throwError (B.restoreWalletSet $ mkCPassPhrase spendingPassword) $ mkCWalletSetInit wSetName mnemonic

-- | Rename a wallet set.
-- Arguments: wallet set id/hash, name
-- Returns json representation of renamed wallet set
-- Example in nodejs:
-- | ```js
-- | > api.renameWalletSet('1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW', 'testing').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > { cwsWalletsNumber: 0,
-- |   cwsWSetMeta: { cwsName: 'testing' },
-- |   cwsPassphraseLU: 1494586629.887586,
-- |   cwsHasPassphrase: true,
-- |   cwsAddress: '1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW' }
-- | ```
renameWalletSet :: forall eff. EffFn2 (ajax :: AJAX | eff) String String (Promise Json)
renameWalletSet = mkEffFn2 \wSetId name -> fromAff <<< map encodeJson $ B.renameWalletSet (mkCAddress wSetId) name

-- | Import a wallet set.
-- Arguments: file path to the wallet set on a filesystem, spending password (set to empty string if you don't want to set password)
-- Returns json representation of imported wallet set
-- Example in nodejs:
-- | > api.importWalletSet('/home/ksaric/projects/haskell/cardano-sl/keys/1.key.hd', '').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > { cwsWalletsNumber: 0,
-- |   cwsWSetMeta: { cwsName: 'Genesis wallet set' },
-- |   cwsPassphraseLU: 1494847007.8911605,
-- |   cwsHasPassphrase: false,
-- |   cwsAddress: '1gCC3J43QAZo3fZiUTuyfYyT8sydFJHdhPnFFmckXL7mV3f' }
importWalletSet :: forall eff. EffFn2 (ajax :: AJAX | eff) String String (Promise Json)
importWalletSet = mkEffFn2 \filePath spendingPassword -> fromAff <<< map encodeJson $ B.importWalletSet (mkCPassPhrase spendingPassword) filePath

-- | Rename a wallet set.
-- Arguments: wallet set id/hash, old spending password (set to empty string if there is no password), new spending password (set to empty string if you want to remove password)
-- Returns json representation of renamed wallet set
-- Example in nodejs:
-- | ```js
-- | > api.changeWalletSetPass('1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW', 'pass', 'pass2').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > {}
-- | ```
changeWalletSetPass :: forall eff. EffFn3 (ajax :: AJAX | eff) String String String (Promise Unit)
changeWalletSetPass = mkEffFn3 \wSetId oldPass newPass -> fromAff $ B.changeWalletSetPass (mkCAddress wSetId) (mkCPassPhrase oldPass) (mkCPassPhrase newPass)

--------------------------------------------------------------------------------
-- Wallets ---------------------------------------------------------------------

-- | Get a wallet.
-- Arguments: wallet object/identifier
-- Returns json representation of a wallet
-- Example in nodejs:
-- | > api.getWallet({"cwaWSAddress": "1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW","cwaIndex": 1759060325}).then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > { cwMeta:
-- |    { cwUnit: 0,
-- |      cwType: 'CWTPersonal',
-- |      cwName: 'drugs',
-- |      cwCurrency: 'ADA',
-- |      cwAssurance: 'CWANormal' },
-- |   cwAddress:
-- |    { cwaWSAddress: '1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW',
-- |      cwaIndex: 1759060325 },
-- |   cwAccounts:
-- |    [ { caAmount: [Object],
-- |        caAddress: '19LniCeNbAxec4FkyxPHCHDzUSnnf4ZymZChq2s9JmYjyr9senVjp4PnbNJ5DPXB8WrWhHCV6Dv2Qv9jdUR5bfNfhdt2vs' } ] }
getWallet :: forall eff. EffFn1 (ajax :: AJAX | eff) Json (Promise Json)
getWallet = mkEffFn1 $ fromAff <<< map encodeJson <<< either (throwError <<< error) B.getWallet <<< decodeJson

-- | Get all wallets.
-- Arguments:
-- Returns json representation of all wallet sets
-- Example in nodejs:
-- | > api.getWallets().then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > [ { cwMeta:
-- |      { cwUnit: 0,
-- |        cwType: 'CWTPersonal',
-- |        cwName: 'drugs',
-- |        cwCurrency: 'ADA',
-- |        cwAssurance: 'CWANormal' },
-- |     cwAddress:
-- |      { cwaWSAddress: '1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW',
-- |        cwaIndex: 1759060325 },
-- |     cwAccounts: [ [Object] ] } ]
getWallets :: forall eff. Eff (ajax :: AJAX | eff) (Promise Json)
getWallets = fromAff $ map encodeJson $ B.getWallets Nothing

-- | Get wallets from specific wallet set.
-- Arguments: address/hash/id of a wallet set
-- Returns json representation of wallets within given wallet set id
-- Example in nodejs:
-- | ```js
-- | > api.getSetWallets('1fbPUqmdG1PdYpKxhw8qYc5hC342W3STosbcZNMqsadodxL').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > [ { cwMeta:
-- |      { cwUnit: 0,
-- |        cwType: 'CWTPersonal',
-- |        cwName: 'Initial wallet',
-- |        cwCurrency: 'ADA',
-- |        cwAssurance: 'CWANormal' },
-- |     cwAddress:
-- |      { cwaWSAddress: '1fbPUqmdG1PdYpKxhw8qYc5hC342W3STosbcZNMqsadodxL',
-- |        cwaIndex: 0 },
-- |     cwAccounts: [ [Object] ] } ]
-- | ```
getSetWallets :: forall eff. EffFn1 (ajax :: AJAX | eff) String (Promise Json)
getSetWallets = mkEffFn1 $ fromAff <<< map encodeJson <<< B.getWallets <<< Just <<< mkCAddress

-- | Get meta information from given wallet
-- Arguments: wallet object/identifier, type, currency, name, assurance, unit
-- Returns json representation of wallets within given wallet id
-- Example in nodejs:
-- | > api.updateWallet({"cwaWSAddress": "1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW","cwaIndex": 1759060325},'CWTPersonal','ADA','Initial wallet','CWANormal',0).then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > { cwMeta:
-- |    { cwUnit: 0,
-- |      cwType: 'CWTPersonal',
-- |      cwName: 'Initial wallet',
-- |      cwCurrency: 'ADA',
-- |      cwAssurance: 'CWANormal' },
-- |   cwAddress:
-- |    { cwaWSAddress: '1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW',
-- |      cwaIndex: 1759060325 },
-- |   cwAccounts:
-- |    [ { caAmount: [Object],
-- |        caAddress: '19LniCeNbAxec4FkyxPHCHDzUSnnf4ZymZChq2s9JmYjyr9senVjp4PnbNJ5DPXB8WrWhHCV6Dv2Qv9jdUR5bfNfhdt2vs' } ] }
updateWallet :: forall eff. EffFn6 (ajax :: AJAX | eff) Json String String String String Int (Promise Json)
updateWallet = mkEffFn6 \wId wType wCurrency wName wAssurance wUnit -> fromAff <<< map encodeJson <<<
    either (throwError <<< error)
        (flip B.updateWallet $ mkCWalletMeta wType wCurrency wName wAssurance wUnit)
        $ decodeJson wId

-- | Creates a new wallet.
-- Arguments: address/hash/id of a wallet set, type, currency, name, mnemonics, spending password (if empty string is given, wallet will be created with no spending password)
-- Returns json representation of newly created wallet
-- Example in nodejs:
-- | > api.newWallet('1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW', 'CWTPersonal', 'ADA', 'drugs', 'pass').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > { cwMeta:
-- |    { cwUnit: 0,
-- |      cwType: 'CWTPersonal',
-- |      cwName: 'drugs',
-- |      cwCurrency: 'ADA',
-- |      cwAssurance: 'CWANormal' },
-- |   cwAddress:
-- |    { cwaWSAddress: '1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW',
-- |      cwaIndex: 293230236 },
-- |   cwAccounts:
-- |    [ { caAmount: [Object],
-- |        caAddress: '19J7gniLEvSDAsHmjTeRUb5wAp8ssFLhUdchabk8FjVBqgDET6LdNa8ZbeZo6tsht4o52hwQ259CLSSoc3iXyEWsZXaEG1' } ] }
newWallet :: forall eff. EffFn5 (ajax :: AJAX | eff) String String String String String
  (Promise Json)
newWallet = mkEffFn5 \wSetId wType wCurrency wName spendingPassword -> fromAff <<< map encodeJson <<<
    B.newWallet (mkCPassPhrase spendingPassword) $ mkCWalletInit wType wCurrency wName (mkCAddress wSetId)

-- | Deletes a wallet.
-- Arguments: wallet object/identifier
-- Returns:
-- Example in nodejs:
-- | > api.deleteWallet({"cwaWSAddress": "1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW","cwaIndex": 293230236}).then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > {}
deleteWallet :: forall eff. EffFn1 (ajax :: AJAX | eff) Json (Promise Unit)
deleteWallet = mkEffFn1 $ fromAff <<< either (throwError <<< error) B.deleteWallet <<< decodeJson

--------------------------------------------------------------------------------
-- Accounts ------------------------------------------------------------------

-- | Creates a new wallet.
-- Arguments: wallet id, spending password
-- Returns json representation of newly created account
-- Example in nodejs:
-- | > api.newAccount({"cwaWSAddress": "1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW","cwaIndex": 1759060325}, 'pass').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > { caAmount: { getCoin: '0' },
-- |   caAddress: '19HWBLraiv8SKtGD3GnTuQZwND8YL5UTTXogPUnpcGzJbNcYHYxm321ovX7NxRsrr1E947AFVFjS4YXtdbgZe8iUgdZCNa' }
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
-- | >  api.isValidAddress('1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > false
isValidAddress :: forall eff. EffFn2 (ajax :: AJAX | eff) String String (Promise Boolean)
isValidAddress = mkEffFn2 \currency -> fromAff <<< B.isValidAddress (mkCCurrency currency)

--------------------------------------------------------------------------------
-- Profiles --------------------------------------------------------------------

-- | Gets user locale.
-- Arguments:
-- Returns users locale
-- Example in nodejs:
-- | > api.getLocale().then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > en-US
getLocale :: forall eff. Eff (ajax :: AJAX | eff) (Promise String)
getLocale = fromAff $ getProfileLocale <$> B.getProfile

-- | Sets user locale.
-- Arguments: new user locale
-- Returns users locale
-- Example in nodejs:
-- | > api.updateLocale('en-US').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > en-US
updateLocale :: forall eff. EffFn1 (ajax :: AJAX | eff) String (Promise String)
updateLocale = mkEffFn1 \locale -> fromAff <<< map getProfileLocale <<< B.updateProfile $ mkCProfile locale

--------------------------------------------------------------------------------
-- Transactions ----------------------------------------------------------------

-- | Creates a new payment.
-- Arguments: wallet object/id, address id/hash, amount to send, spending password (leave empty string if you don't want to use spending password)
-- Returns a created transaction
-- Example in nodejs:
--
newPayment :: forall eff. EffFn4 (ajax :: AJAX | eff) Json String String String (Promise Json)
newPayment = mkEffFn4 \wFrom addrTo amount spendingPassword -> fromAff <<< map encodeJson $ either (throwError <<< error) id
    $ B.newPayment
    <$> (pure $ mkCPassPhrase spendingPassword)
    <*> (decodeJson wFrom)
    <*> (pure $ mkCAddress addrTo)
    <*> (pure $ mkCCoin amount)

-- | Creates a new payment.
-- Arguments: wallet object/id, address id/hash, amount to send, currency, title, description, spending password (leave empty string if you don't want to use spending password)
-- Returns a created transaction
-- Example in nodejs:
--
newPaymentExtended :: forall eff. EffFn7 (ajax :: AJAX | eff) Json String String String String String String (Promise Json)
newPaymentExtended = mkEffFn7 \wFrom addrTo amount curr title desc spendingPassword -> fromAff <<< map encodeJson $ either (throwError <<< error) id
    $ B.newPaymentExtended
    <$> (pure $ mkCPassPhrase spendingPassword)
    <*> (decodeJson wFrom)
    <*> (pure $ mkCAddress addrTo)
    <*> (pure $ mkCCoin amount)
    <*> (pure $ mkCCurrency curr)
    <*> (pure title)
    <*> (pure desc)

-- | Updates transaction meta data.
-- Arguments: wallet object/id, transaction id/hash, currency, title, description, date
-- Returns
-- Example in nodejs:
--
updateTransaction :: forall eff. EffFn6 (ajax :: AJAX | eff) Json String String String String Number (Promise Unit)
updateTransaction = mkEffFn6 \wId ctxId ctmCurrency ctmTitle ctmDescription ctmDate -> fromAff <<< either (throwError <<< error) id
    $ B.updateTransaction
    <$> (decodeJson wId)
    <*> (pure $ mkCTxId ctxId)
    <*> (pure $ mkCTxMeta ctmCurrency ctmTitle ctmDescription ctmDate)

-- | Get transactions of specified wallet
-- Arguments: wallet object/id, skip, limit
-- Returns a pair of transacts retrieved from the history and total number of transactions in specified wallet
-- Example in nodejs:
--
getHistory :: forall eff. EffFn3 (ajax :: AJAX | eff) Json Int Int (Promise Json)
getHistory = mkEffFn3 \wId skip limit -> fromAff <<< map encodeJson $ either (throwError <<< error) id
    $ B.getHistory
    <$> (decodeJson wId)
    <*> (pure $ Just skip)
    <*> (pure $ Just limit)

-- | Gets transactions that match some search criteria
-- Arguments: wallet object/id, search string, skip, limit
-- Returns a pair of transacts retrieved from the history and total number of transactions in specified wallet
-- Example in nodejs:
--
searchHistory :: forall eff. EffFn4 (ajax :: AJAX | eff) Json String Int Int (Promise Json)
searchHistory = mkEffFn4 \wId search skip limit -> fromAff <<< map encodeJson $ either (throwError <<< error) id
    $ B.searchHistory
    <$> (decodeJson wId)
    <*> pure Nothing
    <*> (pure search)
    <*> (pure $ Just skip)
    <*> (pure $ Just limit)

-- | Gets transactions that match some search criteria
-- Arguments: wallet object/id, narrow the search to account hash/id, search string, skip, limit
-- Returns a pair of transacts retrieved from the history and total number of transactions in specified wallet/account
-- Example in nodejs:
--
searchAccountHistory :: forall eff. EffFn5 (ajax :: AJAX | eff) Json String String Int Int (Promise Json)
searchAccountHistory = mkEffFn5 \wId account search skip limit -> fromAff <<< map encodeJson $ either (throwError <<< error) id
    $ B.searchHistory
    <$> (decodeJson wId)
    <*> (pure $ Just $ mkCAddress account)
    <*> (pure search)
    <*> (pure $ Just skip)
    <*> (pure $ Just limit)


--------------------------------------------------------------------------------
-- Updates ---------------------------------------------------------------------

nextUpdate :: forall eff. Eff (ajax :: AJAX | eff) (Promise Json)
nextUpdate = fromAff $ map encodeJson B.nextUpdate

applyUpdate :: forall eff. Eff (ajax :: AJAX | eff) (Promise Unit)
applyUpdate = fromAff B.applyUpdate

--------------------------------------------------------------------------------
-- Redemptions -----------------------------------------------------------------

redeemAda :: forall eff. EffFn3 (ajax :: AJAX, crypto :: Crypto.CRYPTO | eff) String Json String (Promise Json)
redeemAda = mkEffFn3 \seed wId spendingPassword -> fromAff <<< map encodeJson <<< either (throwError <<< error) id
    $ B.redeemAda (mkCPassPhrase spendingPassword)
    <$> mkCWalletRedeem seed
    <$> decodeJson wId

redeemAdaPaperVend :: forall eff. EffFn4 (ajax :: AJAX, crypto :: Crypto.CRYPTO | eff) String String Json String (Promise Json)
redeemAdaPaperVend = mkEffFn4 \seed mnemonic wId spendingPassword -> fromAff <<< map encodeJson <<< either throwError (B.redeemAdaPaperVend $ mkCPassPhrase spendingPassword) $ mkCPaperVendWalletRedeem seed mnemonic =<< lmap error (decodeJson wId)

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

--------------------------------------------------------------------------------
-- Reporting ---------------------------------------------------------------------

reportInit :: forall eff. EffFn2 (ajax :: AJAX, crypto :: Crypto.CRYPTO | eff) Int Int (Promise Unit)
reportInit = mkEffFn2 \total -> fromAff <<< B.reportInit <<< mkCInitialized total

--------------------------------------------------------------------------------
-- Settings ---------------------------------------------------------------------

blockchainSlotDuration :: forall eff. Eff (ajax :: AJAX | eff) (Promise Int)
blockchainSlotDuration = fromAff B.blockchainSlotDuration

systemVersion :: forall eff. Eff (ajax :: AJAX | eff) (Promise Json)
systemVersion = fromAff $ map encodeJson B.systemVersion

syncProgress :: forall eff. Eff (ajax :: AJAX | eff) (Promise Json)
syncProgress = fromAff $ map encodeJson B.syncProgress


--------------------------------------------------------------------------------
-- Mnemonics ---------------------------------------------------------------------

generateMnemonic :: forall eff. Eff (crypto :: Crypto.CRYPTO | eff) String
generateMnemonic = Crypto.generateMnemonic

-- | bip39.validateMnemonic and has at least len words
isValidMnemonic :: forall eff. EffFn2 (crypto :: Crypto.CRYPTO | eff) Int String Boolean
isValidMnemonic = mkEffFn2 \len -> pure <<< either (const false) (const true) <<< mkBackupPhrase len

--------------------------------------------------------------------------------
-- Websockets ---------------------------------------------------------------------

notify :: forall eff. EffFn2 (ref :: REF, ws :: WEBSOCKET, err :: EXCEPTION | eff) (NotifyCb eff) (ErrorCb eff) Unit
notify = mkEffFn2 \messageCb errorCb -> do
    -- TODO (akegalj) grab global (mutable) state of  here
    -- instead of creating newRef
    conn <- newRef WSNotConnected
    openConn $ mkWSState conn messageCb errorCb
