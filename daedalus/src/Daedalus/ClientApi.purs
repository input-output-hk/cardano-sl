module Daedalus.ClientApi where

import Prelude
import Daedalus.BackendApi as B
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (newRef, REF)
import Control.Promise (Promise, fromAff)
import Daedalus.Types (getProfileLocale, mkCAddress, mkCCoin, mkCWalletMeta, mkCTxId, mkCTxMeta, mkCProfile, mkCWalletInit, mkCWalletRedeem, mkBackupPhrase, mkCInitialized, mkCPaperVendWalletRedeem, mkCPassPhrase, mkCWalletSetInit, mkCWalletAddress)
import Daedalus.WS (WSConnection(WSNotConnected), mkWSState, ErrorCb, NotifyCb, openConn)
import Data.Argonaut (Json)
import Data.Argonaut.Generic.Aeson (encodeJson)
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

-- WARNING: this documentation is out of date because of aggresive changes made to the api!

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
-- |   cwsId: '1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW' }
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
-- |     cwsWSetMeta: { cwsUnit: 0, cwsName: 'test', cwsAssurance: 'CWANormal' },
-- |     cwsPassphraseLU: 1495542169.630769,
-- |     cwsId: '1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW',
-- |     cwsHasPassphrase: true,
-- |     cwsAmount: { getCCoin: '0' } },
-- |   { cwsWalletsNumber: 1,
-- |     cwsWSetMeta:
-- |      { cwsUnit: 0,
-- |        cwsName: 'Precreated wallet set full of money',
-- |        cwsAssurance: 'CWANormal' },
-- |     cwsPassphraseLU: 1495541138.013531,
-- |     cwsId: '1gCC3J43QAZo3fZiUTuyfYyT8sydFJHdhPnFFmckXL7mV3f',
-- |     cwsHasPassphrase: false,
-- |     cwsAmount: { getCCoin: '50000' } } ]
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
-- |   cwsId: '1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW' }
-- | ```
newWalletSet :: forall eff . EffFn5 (ajax :: AJAX, crypto :: Crypto.CRYPTO | eff) String String Int String String
  (Promise Json)
newWalletSet = mkEffFn5 \wSetName wsAssurance wsUnit mnemonic spendingPassword -> fromAff <<< map encodeJson <<<
    either throwError (B.newWalletSet $ mkCPassPhrase spendingPassword) $ mkCWalletSetInit wSetName wsAssurance wsUnit mnemonic

-- TODO: note that restoreWalletSet and newWalletSet are the same. They will be unified in future

-- | Restores a new wallet set.
-- Arguments: wallet set name, mnemonics, spending password (set to empty string if you don't want to set password
-- Returns json representation of restored wallet set
-- Example in nodejs:
-- | ```js
-- | >  api.restoreWalletSet('test', 'CWANormal', 0, 'transfer uniform grunt excess six veteran vintage warm confirm vote nephew allow', 'pass').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > Error: ServerError: Pos.Wallet.Web.Error.RequestError "Wallet set with that mnemonics already exists"
-- | 
-- | 
-- | >  api.deleteWalletSet('1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > {}
-- | 
-- | 
-- | >  api.restoreWalletSet('test', 'CWANormal', 0, 'transfer uniform grunt excess six veteran vintage warm confirm vote nephew allow', 'pass').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > { cwsWalletsNumber: 0,
-- |   cwsWSetMeta: { cwsUnit: 0, cwsName: 'test', cwsAssurance: 'CWANormal' },
-- |   cwsPassphraseLU: 1495542169.630769,
-- |   cwsId: '1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW',
-- |   cwsHasPassphrase: true,
-- |   cwsAmount: { getCCoin: '0' } }
-- | ```
restoreWalletSet :: forall eff. EffFn5 (ajax :: AJAX | eff) String String Int String String (Promise Json)
restoreWalletSet = mkEffFn5 \wSetName wsAssurance wsUnit mnemonic spendingPassword -> fromAff <<< map encodeJson <<< either throwError (B.restoreWalletSet $ mkCPassPhrase spendingPassword) $ mkCWalletSetInit wSetName wsAssurance wsUnit mnemonic

-- | Rename a wallet set.
-- Arguments: wallet set id/hash, name
-- Returns json representation of renamed wallet set
-- Example in nodejs:
-- | ```js
-- | >  api.renameWalletSet('1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW', 'testing').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > { cwsWalletsNumber: 0,
-- |   cwsWSetMeta: { cwsUnit: 0, cwsName: 'testing', cwsAssurance: 'CWANormal' },
-- |   cwsPassphraseLU: 1495542169.630769,
-- |   cwsId: '1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW',
-- |   cwsHasPassphrase: true,
-- |   cwsAmount: { getCCoin: '0' } }
-- | 
-- | >  api.renameWalletSet('1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW', 'test').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > { cwsWalletsNumber: 0,
-- |   cwsWSetMeta: { cwsUnit: 0, cwsName: 'test', cwsAssurance: 'CWANormal' },
-- |   cwsPassphraseLU: 1495542169.630769,
-- |   cwsId: '1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW',
-- |   cwsHasPassphrase: true,
-- |   cwsAmount: { getCCoin: '0' } }
-- | ```
renameWalletSet :: forall eff. EffFn2 (ajax :: AJAX | eff) String String (Promise Json)
renameWalletSet = mkEffFn2 \wSetId name -> fromAff <<< map encodeJson $ B.renameWalletSet (mkCAddress wSetId) name

-- | Import a wallet set.
-- Arguments: file path to the wallet set on a filesystem, spending password (set to empty string if you don't want to set password)
-- Returns json representation of imported wallet set
-- Example in nodejs:
-- | ```js
-- | > api.importWalletSet('/home/akegalj/projects/serokell/cardano-sl/keys/2.key.hd', '').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > { cwsWalletsNumber: 0,
-- |   cwsWSetMeta:
-- |    { cwsUnit: 0,
-- |      cwsName: 'Genesis wallet set',
-- |      cwsAssurance: 'CWANormal' },
-- |   cwsPassphraseLU: 1495545014.377285,
-- |   cwsId: '1feqWtoyaxFyvKQFWo46vHSc7urynGaRELQE62T74Y3RBs8',
-- |   cwsHasPassphrase: false,
-- |   cwsAmount: { getCCoin: '0' } }
-- | ```
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

-- | Deletes a wallet set.
-- Arguments: wallet set identifier
-- Returns:
-- Example in nodejs:
-- | ```js
-- | > api.deleteWalletSet('1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > {}
-- | ```
deleteWalletSet :: forall eff. EffFn1 (ajax :: AJAX | eff) String (Promise Unit)
deleteWalletSet = mkEffFn1 $ fromAff <<< B.deleteWalletSet <<< mkCAddress

--------------------------------------------------------------------------------
-- Wallets ---------------------------------------------------------------------

-- | Get a wallet.
-- Arguments: wallet identifier
-- Returns json representation of a wallet
-- Example in nodejs:
-- | ```js
-- | > api.getWallet('1feqWtoyaxFyvKQFWo46vHSc7urynGaRELQE62T74Y3RBs8@2147483648').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > { cwMeta: { cwName: 'Genesis wallet' },
-- |   cwId: '1feqWtoyaxFyvKQFWo46vHSc7urynGaRELQE62T74Y3RBs8@2147483648',
-- |   cwAmount: { getCCoin: '50000' },
-- |   cwAccounts:
-- |    [ { caId: '19FLnEFfkaLsZqBqYHjPmCypZNHNZ7SBfMsntKgspqA96F18s6eeDy5GYjHmwXSECG6jRqWh9qqEAicpEXrNhpb8PuRNVL',
-- |        caAmount: [Object] } ] }
-- | ```
getWallet :: forall eff. EffFn1 (ajax :: AJAX | eff) String (Promise Json)
getWallet = mkEffFn1 $ fromAff <<< map encodeJson <<< B.getWallet <<< mkCWalletAddress

-- | Get all wallets.
-- Arguments:
-- Returns json representation of all wallet sets
-- Example in nodejs:
-- | ```js
-- | > api.getWallets().then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > [ { cwMeta: { cwName: 'Genesis wallet' },
-- |     cwId: '1feqWtoyaxFyvKQFWo46vHSc7urynGaRELQE62T74Y3RBs8@2147483648',
-- |     cwAmount: { getCCoin: '50000' },
-- |     cwAccounts: [ [Object] ] },
-- |   { cwMeta: { cwName: 'Initial wallet' },
-- |     cwId: '1gCC3J43QAZo3fZiUTuyfYyT8sydFJHdhPnFFmckXL7mV3f@2147483648',
-- |     cwAmount: { getCCoin: '50000' },
-- |     cwAccounts: [ [Object] ] } ]
-- | ```
getWallets :: forall eff. Eff (ajax :: AJAX | eff) (Promise Json)
getWallets = fromAff $ map encodeJson $ B.getWallets Nothing

-- | Get wallets from specific wallet set.
-- Arguments: address/hash/id of a wallet set
-- Returns json representation of wallets within given wallet set id
-- Example in nodejs:
-- | ```js
-- | > api.getSetWallets('1gCC3J43QAZo3fZiUTuyfYyT8sydFJHdhPnFFmckXL7mV3f').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > [ { cwMeta: { cwName: 'Initial wallet' },
-- |     cwId: '1gCC3J43QAZo3fZiUTuyfYyT8sydFJHdhPnFFmckXL7mV3f@2147483648',
-- |     cwAmount: { getCCoin: '50000' },
-- |     cwAccounts: [ [Object] ] } ]
-- | ```
getSetWallets :: forall eff. EffFn1 (ajax :: AJAX | eff) String (Promise Json)
getSetWallets = mkEffFn1 $ fromAff <<< map encodeJson <<< B.getWallets <<< Just <<< mkCAddress

-- | Get meta information from given wallet
-- Arguments: wallet object/identifier, type, currency, name, assurance, unit
-- Returns json representation of wallets within given wallet id
-- Example in nodejs:
-- | ```js
-- | > api.updateWallet('1gCC3J43QAZo3fZiUTuyfYyT8sydFJHdhPnFFmckXL7mV3f@2147483648','CWTPersonal','ADA','Initial wallet','CWANormal',0).then(console.log)
-- | Promise { <pending> }
-- | > { cwMeta: { cwName: 'CWTPersonal' },
-- |   cwId: '1gCC3J43QAZo3fZiUTuyfYyT8sydFJHdhPnFFmckXL7mV3f@2147483648',
-- |   cwAmount: { getCCoin: '50000' },
-- |   cwAccounts:
-- |    [ { caId: '19Fv6JWbdLXRXqew721u2GEarEwc8rcfpAqsriRFPameyCkQLHsNDKQRpwsM7W1M587CiswPuY27cj7RUvNXcZWgTbPByq',
-- |        caAmount: [Object] } ] }
-- | ```
updateWallet :: forall eff. EffFn2 (ajax :: AJAX | eff) String String (Promise Json)
updateWallet = mkEffFn2 \wId wName -> fromAff <<< map encodeJson <<<
    B.updateWallet (mkCWalletAddress wId) $ mkCWalletMeta wName

-- | Creates a new wallet.
-- Arguments: address/hash/id of a wallet set, type, currency, name, mnemonics, spending password (if empty string is given, wallet will be created with no spending password)
-- Returns json representation of newly created wallet
-- Example in nodejs:
-- | ```js
-- | > api.newWallet('1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW', 'trips', 'pass').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > { cwMeta: { cwName: 'trips' },
-- |   cwId: '1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW@3190108780',
-- |   cwAmount: { getCCoin: '0' },
-- |   cwAccounts:
-- |    [ { caId: '19M3DbeepAzN6xzSSErL8pk1JQA8oFkgE9L6LZfKXMiNpoPDjfDpJjWa3Jis1oCZVGMo1pM8tio2wifuhDPWzwCWS6sZfX',
-- |        caAmount: [Object] } ] }
-- | ```
newWallet :: forall eff. EffFn3 (ajax :: AJAX | eff) String String String
  (Promise Json)
newWallet = mkEffFn3 \wSetId wName spendingPassword -> fromAff <<< map encodeJson <<<
    B.newWallet (mkCPassPhrase spendingPassword) $ mkCWalletInit wName (mkCAddress wSetId)

-- | Deletes a wallet.
-- Arguments: wallet object/identifier
-- Returns:
-- Example in nodejs:
-- | ```js
-- | >  api.deleteWallet('1feqWtoyaxFyvKQFWo46vHSc7urynGaRELQE62T74Y3RBs8@2147483648').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > {}
-- | ```
deleteWallet :: forall eff. EffFn1 (ajax :: AJAX | eff) String (Promise Unit)
deleteWallet = mkEffFn1 $ fromAff <<< B.deleteWallet <<< mkCWalletAddress

--------------------------------------------------------------------------------
-- Accounts ------------------------------------------------------------------

-- | Creates a new account in the wallet.
-- Arguments: wallet id, spending password
-- Returns json representation of newly created account
-- Example in nodejs:
-- | ```js
-- | > api.newAccount('1gCC3J43QAZo3fZiUTuyfYyT8sydFJHdhPnFFmckXL7mV3f@2147483648', '').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > { caId: '19N52o4RrzEo6AxRzawAkbuMtnqPjrgat1USDMaRQG3uK46b7bNrpxMSLgd1sxvPUPFbGnmj9Kmj2Fb8H5W5Ez7g6voZMy',
-- |   caAmount: { getCCoin: '0' } }
-- | ```
newAccount :: forall eff . EffFn2 (ajax :: AJAX | eff) String String
  (Promise Json)
newAccount = mkEffFn2 \wId spendingPassword -> fromAff <<< map encodeJson <<<
    B.newAccount (mkCPassPhrase spendingPassword) $ mkCWalletAddress wId

--------------------------------------------------------------------------------
-- Addresses ------------------------------------------------------------------

-- | Checks is some string correct representation of a valid address
-- Arguments: currency for which we are checking address, string representation of an address to check
-- Returns true if address is valid in specific currency or false otherwise
-- Example in nodejs:
-- | ```js
-- | > api.isValidAddress('1feqWtoyaxFyvKQFWo46vHSc7urynGaRELQE62T74Y3RBs8').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > true
-- |
-- | > api.isValidAddress('19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwMYbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > true
-- |
-- | > api.isValidAddress('19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwMYbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9g').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > false
-- |
-- | > api.isValidAddress('1feqWtoyaxFyvKQFWo46vHSc7urynGaRELQE62T74Y3RBs9').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > false
isValidAddress :: forall eff. EffFn1 (ajax :: AJAX | eff) String (Promise Boolean)
isValidAddress = mkEffFn1 $ fromAff <<< B.isValidAddress

--------------------------------------------------------------------------------
-- Profiles --------------------------------------------------------------------

-- | Gets user locale.
-- Arguments:
-- Returns users locale
-- Example in nodejs:
-- | ```js
-- | > api.getLocale().then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > en-US
-- | ```
getLocale :: forall eff. Eff (ajax :: AJAX | eff) (Promise String)
getLocale = fromAff $ getProfileLocale <$> B.getProfile

-- | Sets user locale.
-- Arguments: new user locale
-- Returns users locale
-- Example in nodejs:
-- | ```js
-- | > api.updateLocale('en-US').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > en-US
-- | ```
updateLocale :: forall eff. EffFn1 (ajax :: AJAX | eff) String (Promise String)
updateLocale = mkEffFn1 \locale -> fromAff <<< map getProfileLocale <<< B.updateProfile $ mkCProfile locale

--------------------------------------------------------------------------------
-- Transactions ----------------------------------------------------------------

-- | Creates a new payment.
-- Arguments: wallet object/id, address id/hash, amount to send, spending password (leave empty string if you don't want to use spending password)
-- Returns a created transaction
-- Example in nodejs:
-- | ```js
-- | > api.newPayment('1gCC3J43QAZo3fZiUTuyfYyT8sydFJHdhPnFFmckXL7mV3f@2147483648', '19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwMYbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs', 1, '').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > { ctOutputAddrs:
-- |    [ '19FQ6bXnyQaTS2JximL4nQJK9BYAvrBe46532WsWHi8SW7kVwqza61UY3iLzYeKMi9akhsx6f5dhA5UiRgoAFRw8dGDuCV',
-- |      '19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwMYbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs' ],
-- |   ctMeta:
-- |    { ctmTitle: '',
-- |      ctmDescription: '',
-- |      ctmDate: 1495462376.0430539,
-- |      ctmCurrency: 'ADA' },
-- |   ctInputAddrs: [ '19Fv6JWbdLXRXqew721u2GEarEwc8rcfpAqsriRFPameyCkQLHsNDKQRpwsM7W1M587CiswPuY27cj7RUvNXcZWgTbPByq' ],
-- |   ctId: '7ad2d409d4e6cbb9c4eec6e76e54addfddb1a0bad9cdc4e46e4d786991a9bda3',
-- |   ctConfirmations: 0,
-- |   ctAmount: { getCoin: '50000' } }
-- | ```
newPayment :: forall eff. EffFn4 (ajax :: AJAX | eff) String String String String (Promise Json)
newPayment = mkEffFn4 \wFrom addrTo amount spendingPassword -> fromAff <<< map encodeJson $
    B.newPayment
    (mkCPassPhrase spendingPassword)
    (mkCWalletAddress wFrom)
    (mkCAddress addrTo)
    (mkCCoin amount)

-- | Creates a new payment.
-- Arguments: wallet object/id, address id/hash, amount to send, currency, title, description, spending password (leave empty string if you don't want to use spending password)
-- Returns a created transaction
-- Example in nodejs:
-- | ```js
-- | > api.newPaymentExtended('1gCC3J43QAZo3fZiUTuyfYyT8sydFJHdhPnFFmckXL7mV3f@2147483648', '19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwMYbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs', 10, 'ADA', 'Programming task', 'Programming the new brilliant cryptocurrency', '').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > { ctOutputAddrs:
-- |    [ '19JiAGXcsH4WhLcUTbiPCFdmkdLW9LHG2uMCtPumBnSp4FQVpwiktua2y9PbKQFPi5ftUjyn9p5T61p3QjsCECu3h24xBg',
-- |      '19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwMYbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs' ],
-- |   ctMeta:
-- |    { ctmTitle: 'Programming task',
-- |      ctmDescription: 'Programming the new brilliant cryptocurrency',
-- |      ctmDate: 1495462417.3288133,
-- |      ctmCurrency: 'ADA' },
-- |   ctInputAddrs: [ '19FQ6bXnyQaTS2JximL4nQJK9BYAvrBe46532WsWHi8SW7kVwqza61UY3iLzYeKMi9akhsx6f5dhA5UiRgoAFRw8dGDuCV' ],
-- |   ctId: '0295cf235dce9ead02134d1114135c47710fc273593f4324b595e93870979c5f',
-- |   ctConfirmations: 0,
-- |   ctAmount: { getCoin: '49999' } }
-- | ```
newPaymentExtended :: forall eff. EffFn6 (ajax :: AJAX | eff) String String String String String String (Promise Json)
newPaymentExtended = mkEffFn6 \wFrom addrTo amount title desc spendingPassword -> fromAff <<< map encodeJson $
    B.newPaymentExtended
    (mkCPassPhrase spendingPassword)
    (mkCWalletAddress wFrom)
    (mkCAddress addrTo)
    (mkCCoin amount)
    title
    desc

-- | Updates transaction meta data.
-- Arguments: wallet object/id, transaction id/hash, currency, title, description, date
-- Returns
-- Example in nodejs:
-- | ```js
-- | > api.updateTransaction('1gCC3J43QAZo3fZiUTuyfYyT8sydFJHdhPnFFmckXL7mV3f@2147483648', 'cc7576fef33a4a60865f9149792fa7359f44eca6745aeb1ba751185bab9bd7ac', 'ADA', 'Manager task', 'Managing people and other stuff', 1494935150.0468155).then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > {}
-- | ```
updateTransaction :: forall eff. EffFn5 (ajax :: AJAX | eff) String String String String Number (Promise Unit)
updateTransaction = mkEffFn5 \wId ctxId ctmTitle ctmDescription ctmDate -> fromAff $
    B.updateTransaction
    (mkCWalletAddress wId)
    (mkCTxId ctxId)
    (mkCTxMeta ctmTitle ctmDescription ctmDate)

-- | Get transactions of specified wallet
-- Arguments: wallet object/id, skip, limit
-- Returns a pair of transacts retrieved from the history and total number of transactions in specified wallet
-- Example in nodejs:
-- | ```js
-- | > api.getHistory('1gCC3J43QAZo3fZiUTuyfYyT8sydFJHdhPnFFmckXL7mV3f@2147483648', 0, 10).then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > [ [ { ctOutputAddrs: [Object],
-- |       ctMeta: [Object],
-- |       ctInputAddrs: [Object],
-- |       ctId: '7ad2d409d4e6cbb9c4eec6e76e54addfddb1a0bad9cdc4e46e4d786991a9bda3',
-- |       ctConfirmations: 0,
-- |       ctAmount: [Object] },
-- |     { ctOutputAddrs: [Object],
-- |       ctMeta: [Object],
-- |       ctInputAddrs: [Object],
-- |       ctId: '0295cf235dce9ead02134d1114135c47710fc273593f4324b595e93870979c5f',
-- |       ctConfirmations: 0,
-- |       ctAmount: [Object] } ],
-- |   2 ]
-- | ```
getHistory :: forall eff. EffFn3 (ajax :: AJAX | eff) String Int Int (Promise Json)
getHistory = mkEffFn3 \wId skip limit -> fromAff <<< map encodeJson $
    B.getHistory
    (mkCWalletAddress wId)
    (Just skip)
    (Just limit)

-- | Gets transactions that match some search criteria
-- Arguments: wallet object/id, search string, skip, limit
-- Returns a pair of transacts retrieved from the history and total number of transactions in specified wallet
-- Example in nodejs:
-- | ```js
-- | > api.searchHistory('1gCC3J43QAZo3fZiUTuyfYyT8sydFJHdhPnFFmckXL7mV3f@2147483648', 'task', 0, 10).then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > [ [ { ctOutputAddrs: [Object],
-- |       ctMeta: [Object],
-- |       ctInputAddrs: [Object],
-- |       ctId: '0295cf235dce9ead02134d1114135c47710fc273593f4324b595e93870979c5f',
-- |       ctConfirmations: 0,
-- |       ctAmount: [Object] } ],
-- |   2 ]
-- | ```
searchHistory :: forall eff. EffFn4 (ajax :: AJAX | eff) String String Int Int (Promise Json)
searchHistory = mkEffFn4 \wId search skip limit -> fromAff <<< map encodeJson $
    B.searchHistory
    (mkCWalletAddress wId)
    Nothing
    search
    (Just skip)
    (Just limit)

-- | Gets transactions that match some search criteria
-- Arguments: wallet object/id, narrow the search to account hash/id, search string, skip, limit
-- Returns a pair of transacts retrieved from the history and total number of transactions in specified wallet/account
-- Example in nodejs:
-- | ```js
-- | > api.searchAccountHistory('1gCC3J43QAZo3fZiUTuyfYyT8sydFJHdhPnFFmckXL7mV3f@2147483648', '19GfdoC3ytim4rsTXRMp5At6Bmt512XkcbUwGV69jqWvuRhU5HS5gNAbQ6JpUDavDiKRNMb9iyp6vKUCdJiaKLJdhmcQN9', '', 0, 10).then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > [ [ { ctOutputAddrs: [Object],
-- |       ctMeta: [Object],
-- |       ctInputAddrs: [Object],
-- |       ctId: '7ad2d409d4e6cbb9c4eec6e76e54addfddb1a0bad9cdc4e46e4d786991a9bda3',
-- |       ctConfirmations: 0,
-- |       ctAmount: [Object] },
-- |     { ctOutputAddrs: [Object],
-- |       ctMeta: [Object],
-- |       ctInputAddrs: [Object],
-- |       ctId: '0295cf235dce9ead02134d1114135c47710fc273593f4324b595e93870979c5f',
-- |       ctConfirmations: 0,
-- |       ctAmount: [Object] } ],
-- |   2 ]
-- | ```
searchAccountHistory :: forall eff. EffFn5 (ajax :: AJAX | eff) String String String Int Int (Promise Json)
searchAccountHistory = mkEffFn5 \wId account search skip limit -> fromAff <<< map encodeJson $
    B.searchHistory
    (mkCWalletAddress wId)
    (Just $ mkCAddress account)
    search
    (Just skip)
    (Just limit)


--------------------------------------------------------------------------------
-- Updates ---------------------------------------------------------------------

-- Example in nodejs:
-- | ```js
-- | > api.nextUpdate().then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > Error: ServerError: Pos.Wallet.Web.Error.Internal "No updates available"
-- |     at Object.exports.error (/home/ksaric/projects/haskell/cardano-sl/daedalus/output/Control.Monad.Eff.Exception/foreign.js:8:10)
-- |     at mkServerError (/home/ksaric/projects/haskell/cardano-sl/daedalus/output/Daedalus.BackendApi/index.js:94:44)
-- |     at /home/ksaric/projects/haskell/cardano-sl/daedalus/output/Data.Either/index.js:256:33
-- |     at /home/ksaric/projects/haskell/cardano-sl/daedalus/output/Data.Either/index.js:230:24
-- |     at /home/ksaric/projects/haskell/cardano-sl/daedalus/output/Daedalus.BackendApi/index.js:102:127
-- |     at /home/ksaric/projects/haskell/cardano-sl/daedalus/output/Daedalus.BackendApi/index.js:128:207
-- |     at /home/ksaric/projects/haskell/cardano-sl/daedalus/output/Control.Monad.Aff/foreign.js:182:21
-- |     at /home/ksaric/projects/haskell/cardano-sl/daedalus/output/Control.Monad.Aff/foreign.js:147:5
-- |     at /home/ksaric/projects/haskell/cardano-sl/daedalus/output/Control.Monad.Aff/foreign.js:176:17
-- |     at /home/ksaric/projects/haskell/cardano-sl/daedalus/output/Control.Monad.Aff/foreign.js:182:25
-- | ```
nextUpdate :: forall eff. Eff (ajax :: AJAX | eff) (Promise Json)
nextUpdate = fromAff $ map encodeJson B.nextUpdate

-- Example in nodejs:
-- | ```js
-- | > api.applyUpdate().then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > {}
-- | ```
applyUpdate :: forall eff. Eff (ajax :: AJAX | eff) (Promise Unit)
applyUpdate = fromAff B.applyUpdate

--------------------------------------------------------------------------------
-- Redemptions -----------------------------------------------------------------

-- TODO: this endpoint wasn’t verified yet! Need to be tested with genesis block prepared for redeeming!
-- Example in nodejs:
-- | ```js
-- | > api.redeemAda('lwIF94R9AYRwBy0BkVVpLhwtsG3CmqDvMahlQr3xKEY=', '1gCC3J43QAZo3fZiUTuyfYyT8sydFJHdhPnFFmckXL7mV3f@2147483648', '').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > Error: ServerError: Pos.Wallet.Web.Error.RequestError "Cannot send redemption transaction: Failed to prepare inputs!"
-- |     at Object.exports.error (/home/ksaric/projects/haskell/cardano-sl/daedalus/output/Control.Monad.Eff.Exception/foreign.js:8:10)
-- |     at mkServerError (/home/ksaric/projects/haskell/cardano-sl/daedalus/output/Daedalus.BackendApi/index.js:94:44)
-- |     at /home/ksaric/projects/haskell/cardano-sl/daedalus/output/Data.Either/index.js:256:33
-- |     at /home/ksaric/projects/haskell/cardano-sl/daedalus/output/Data.Either/index.js:230:24
-- |     at /home/ksaric/projects/haskell/cardano-sl/daedalus/output/Daedalus.BackendApi/index.js:102:127
-- |     at /home/ksaric/projects/haskell/cardano-sl/daedalus/output/Daedalus.BackendApi/index.js:128:207
-- |     at /home/ksaric/projects/haskell/cardano-sl/daedalus/output/Control.Monad.Aff/foreign.js:182:21
-- |     at /home/ksaric/projects/haskell/cardano-sl/daedalus/output/Control.Monad.Aff/foreign.js:147:5
-- |     at /home/ksaric/projects/haskell/cardano-sl/daedalus/output/Control.Monad.Aff/foreign.js:176:17
-- |     at /home/ksaric/projects/haskell/cardano-sl/daedalus/output/Control.Monad.Aff/foreign.js:182:25
-- | ```
redeemAda :: forall eff. EffFn3 (ajax :: AJAX, crypto :: Crypto.CRYPTO | eff) String String String (Promise Json)
redeemAda = mkEffFn3 \seed wId spendingPassword -> fromAff <<< map encodeJson $
    B.redeemAda
    (mkCPassPhrase spendingPassword)
    (mkCWalletRedeem seed $ mkCWalletAddress wId)

-- TODO: this endpoint wasn’t verified yet! Need to be tested with genesis block prepared for redeeming!
-- Example in nodejs:
-- | ```js
-- | > api.redeemAdaPaperVend('lwIF94R9AYRwBy0BkVVpLhwtsG3CmqDvMahlQr3xKEY=', 'transfer uniform grunt excess six veteran vintage warm confirm vote nephew allow', '1gCC3J43QAZo3fZiUTuyfYyT8sydFJHdhPnFFmckXL7mV3f@2147483648', '').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > Error: Invalid mnemonic: mnemonic should have at least 12 words
-- |     at Object.exports.error (/home/ksaric/projects/haskell/cardano-sl/daedalus/output/Control.Monad.Eff.Exception/foreign.js:8:10)
-- |     at /home/ksaric/projects/haskell/cardano-sl/daedalus/output/Daedalus.Types/index.js:152:72
-- |     at /home/ksaric/projects/haskell/cardano-sl/daedalus/output/Daedalus.Types/index.js:165:91
-- |     at /home/ksaric/projects/haskell/cardano-sl/daedalus/output/Daedalus.Types/index.js:182:98
-- |     at /home/ksaric/projects/haskell/cardano-sl/daedalus/output/Daedalus.ClientApi/index.js:109:407
-- |     at Object.redeemAdaPaperVend (/home/ksaric/projects/haskell/cardano-sl/daedalus/output/Data.Function.Eff/foreign.js:21:23)
-- |     at repl:1:5
-- |     at ContextifyScript.Script.runInThisContext (vm.js:23:33)
-- |     at REPLServer.defaultEval (repl.js:336:29)
-- |     at bound (domain.js:280:14)
-- | ```
-- NOTE: if you will be bumping bip39 to >=2.2.0 be aware of https://issues.serokell.io/issue/VD-95 . In this case you will have to modify how we validate paperVendMnemonics.
redeemAdaPaperVend :: forall eff. EffFn4 (ajax :: AJAX, crypto :: Crypto.CRYPTO | eff) String String String String (Promise Json)
redeemAdaPaperVend = mkEffFn4 \seed mnemonic wId spendingPassword -> fromAff <<< map encodeJson <<< either throwError (B.redeemAdaPaperVend $ mkCPassPhrase spendingPassword) $ mkCPaperVendWalletRedeem seed mnemonic $ mkCWalletAddress wId

-- Valid redeem code is base64 encoded 32byte data
-- NOTE: this method handles both base64 and base64url base on rfc4648: see more https://github.com/menelaos/purescript-b64/blob/59e2e9189358a4c8e3eef8662ca281906844e783/src/Data/String/Base64.purs#L182
-- Example in nodejs:
-- | ```js
-- | > api.isValidRedemptionKey('lwIF94R9AYRwBy0BkVVpLhwtsG3CmqDvMahlQr3xKEY=')
-- | true
-- | ```
isValidRedemptionKey :: String -> Boolean
isValidRedemptionKey code = either (const false) (const $ endsWithEqual && 44 == length code) $ B64.decode code
  where
    -- Because it is 32byte base64 encoded
    endsWithEqual = isJust $ stripSuffix (Pattern "=") code

-- Valid paper vend key is base58 encoded 32byte data
-- Example in nodejs:
-- | ```js
-- | > api.isValidPaperVendRedemptionKey('lwIF94R9AYRwBy0BkVVpLhwtsG3CmqDvMahlQr3xKEY=')
-- | false
-- | ```
isValidPaperVendRedemptionKey :: String -> Boolean
isValidPaperVendRedemptionKey code = maybe false ((==) 32 <<< A.length) $ B58.decode code

--------------------------------------------------------------------------------
-- Reporting ---------------------------------------------------------------------

-- Example in nodejs:
-- | ```js
-- | > api.reportInit(1, 1).then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > {}
-- | ```
reportInit :: forall eff. EffFn2 (ajax :: AJAX, crypto :: Crypto.CRYPTO | eff) Int Int (Promise Unit)
reportInit = mkEffFn2 \total -> fromAff <<< B.reportInit <<< mkCInitialized total

--------------------------------------------------------------------------------
-- Settings ---------------------------------------------------------------------

-- Example in nodejs:
-- | ```js
-- | > api.blockchainSlotDuration().then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > 7000
-- | ```
blockchainSlotDuration :: forall eff. Eff (ajax :: AJAX | eff) (Promise Int)
blockchainSlotDuration = fromAff B.blockchainSlotDuration

-- Example in nodejs:
-- | ```js
-- | > api.systemVersion().then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > { svNumber: 0, svAppName: { getApplicationName: 'cardano-sl' } }
-- | ```
systemVersion :: forall eff. Eff (ajax :: AJAX | eff) (Promise Json)
systemVersion = fromAff $ map encodeJson B.systemVersion

-- Example in nodejs:
-- | ```js
-- | > api.syncProgress().then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > { _spPeers: 0,
-- |   _spNetworkCD: null,
-- |   _spLocalCD: { getChainDifficulty: 4 } }
-- | ```
syncProgress :: forall eff. Eff (ajax :: AJAX | eff) (Promise Json)
syncProgress = fromAff $ map encodeJson B.syncProgress

--------------------------------------------------------------------------------
-- Mnemonics ---------------------------------------------------------------------

-- Example in nodejs:
-- | ```js
-- | > api.generateMnemonic()
-- | 'obtain divide top receive purchase shuffle opinion circle future spare athlete quantum'
-- | ```
generateMnemonic :: forall eff. Eff (crypto :: Crypto.CRYPTO | eff) String
generateMnemonic = Crypto.generateMnemonic

-- | bip39.validateMnemonic and has at least len words
-- Example in nodejs:
-- | ```js
-- | > api.isValidMnemonic(12, 'obtain divide top receive purchase shuffle opinion circle future spare athlete quantum')
-- | true
-- | ```
-- NOTE: if you will be bumping bip39 to >=2.2.0 be aware of https://issues.serokell.io/issue/VD-95 . In this case you will have to modify how we validate paperVendMnemonics.
isValidMnemonic :: forall eff. EffFn2 (crypto :: Crypto.CRYPTO | eff) Int String Boolean
isValidMnemonic = mkEffFn2 \len -> pure <<< either (const false) (const true) <<< mkBackupPhrase len

--------------------------------------------------------------------------------
-- Websockets ---------------------------------------------------------------------

-- Example for testing
-- | > wscat -c ws://127.0.0.1:8090
-- |
-- | connected (press CTRL+C to quit)
-- |
-- | < {"tag":"ConnectionOpened"}
-- |
-- | < {"tag":"NetworkDifficultyChanged","contents":{"getChainDifficulty":1}}
-- | < {"tag":"LocalDifficultyChanged","contents":{"getChainDifficulty":1}}
-- | < {"tag":"NetworkDifficultyChanged","contents":{"getChainDifficulty":2}}
-- | < {"tag":"LocalDifficultyChanged","contents":{"getChainDifficulty":2}}
-- | < {"tag":"NetworkDifficultyChanged","contents":{"getChainDifficulty":3}}
-- | < {"tag":"LocalDifficultyChanged","contents":{"getChainDifficulty":3}}
-- | < {"tag":"NetworkDifficultyChanged","contents":{"getChainDifficulty":4}}
-- | < {"tag":"LocalDifficultyChanged","contents":{"getChainDifficulty":4}}
-- | ```
notify :: forall eff. EffFn2 (ref :: REF, ws :: WEBSOCKET, err :: EXCEPTION | eff) (NotifyCb eff) (ErrorCb eff) Unit
notify = mkEffFn2 \messageCb errorCb -> do
    -- TODO (akegalj) grab global (mutable) state of  here
    -- instead of creating newRef
    conn <- newRef WSNotConnected
    openConn $ mkWSState conn messageCb errorCb
