module Daedalus.ClientApi where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Uncurried (EffFn1, mkEffFn1, EffFn2, mkEffFn2, EffFn4, mkEffFn4, EffFn5, mkEffFn5, EffFn3, mkEffFn3, EffFn6, mkEffFn6)
import Control.Monad.Error.Class (throwError)
import Control.Promise (Promise, fromAff)
import Daedalus.BackendApi as B
import Daedalus.Types (getProfileLocale, mkBackupPhrase, mkCAccountId, mkCAccountInit, mkCAccountMeta, mkCCoin, mkCId, mkCInitialized, mkCPaperVendWalletRedeem, mkCPassPhrase, mkCProfile, mkCTxId, mkCTxMeta, mkCWalletInit, mkCWalletMeta, mkCWalletRedeem, optionalString, CFilePath (..))
import Daedalus.Crypto as Crypto
import Daedalus.TLS (TLSOptions, FS, initTLS)
import Data.Argonaut (Json)
import Data.Argonaut.Generic.Aeson (encodeJson)
import Data.Array as A
import Data.Base58 as B58
import Data.Either (either)
import Data.Foreign (Foreign)
import Data.Maybe (isJust, maybe, Maybe(..))
import Data.String (length, stripSuffix, Pattern(..))
import Data.String.Base64 as B64
import Node.Buffer (Buffer)
import Node.HTTP (HTTP)

-- TLS

tlsInit :: forall eff. EffFn1 (fs :: FS, err :: EXCEPTION | eff) Buffer TLSOptions
tlsInit = mkEffFn1 initTLS



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
testReset :: forall eff. EffFn1 (http :: HTTP, exception :: EXCEPTION | eff) TLSOptions (Promise Unit)
testReset = mkEffFn1 $ fromAff <<< B.testReset

--------------------------------------------------------------------------------
-- Wallet Sets ---------------------------------------------------------------------

-- | Gets specified wallet set
-- Arguments: wallet set id/hash
-- Returns json representation of requested wallet set
-- Example in nodejs:
-- | ```js
-- | > api.getWallet('1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > { cwAccountsNumber: 0,
-- |   cwMeta: { cwName: 'test' },
-- |   cwPassphraseLU: 1494583348.3572557,
-- |   cwHasPassphrase: true,
-- |   cwId: '1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW' }
-- | ```
getWallet :: forall eff. EffFn2 (http :: HTTP, exception :: EXCEPTION | eff) TLSOptions String (Promise Json)
getWallet = mkEffFn2 $ \tls -> fromAff <<< map encodeJson <<< B.getWallet tls <<< mkCId

-- | Gets all wallet sets
-- Arguments:
-- Returns json representation of all wallet sets
-- Example in nodejs:
-- | ```js
-- | > api.getWallets().then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > [ { cwAccountsNumber: 0,
-- |     cwMeta: { cwUnit: 0, cwName: 'test', cwAssurance: 'CWANormal' },
-- |     cwPassphraseLU: 1495542169.630769,
-- |     cwId: '1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW',
-- |     cwHasPassphrase: true,
-- |     cwAmount: { getCCoin: '0' } },
-- |   { cwAccountsNumber: 1,
-- |     cwMeta:
-- |      { cwUnit: 0,
-- |        cwName: 'Precreated wallet set full of money',
-- |        cwAssurance: 'CWANormal' },
-- |     cwPassphraseLU: 1495541138.013531,
-- |     cwId: '1gCC3J43QAZo3fZiUTuyfYyT8sydFJHdhPnFFmckXL7mV3f',
-- |     cwHasPassphrase: false,
-- |     cwAmount: { getCCoin: '50000' } } ]
-- | ```
getWallets :: forall eff. EffFn1 (http :: HTTP, exception :: EXCEPTION | eff) TLSOptions (Promise Json)
getWallets = mkEffFn1 $ fromAff <<< map encodeJson <<< B.getWallets

-- | Creates a new wallet set.
-- Arguments: wallet set name, mnemonics, spending password (set to empty string if you don't want to set password)
-- Returns json representation of created wallet set
-- Example in nodejs:
-- | ```js
-- | > api.newWallet('test', 'CWANormal', 0, 'transfer uniform grunt excess six veteran vintage warm confirm vote nephew allow', 'pass').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > { cwAccountsNumber: 0,
-- |   cwMeta: { cwName: 'test' },
-- |   cwPassphraseLU: 1494583348.3572557,
-- |   cwHasPassphrase: true,
-- |   cwId: '1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW' }
-- | ```
newWallet
    :: forall eff.
    EffFn6 (http :: HTTP, err :: EXCEPTION, exception :: EXCEPTION, crypto :: Crypto.CRYPTO | eff)
    TLSOptions
    String
    String
    Int
    String
    Foreign
    (Promise Json)
newWallet = mkEffFn6 cNewWallet
  where
    cNewWallet
        :: TLSOptions
        -> String
        -> String
        -> Int
        -> String
        -> Foreign
        -> Eff (http :: HTTP, err :: EXCEPTION, exception :: EXCEPTION, crypto :: Crypto.CRYPTO | eff) (Promise Json)
    cNewWallet tls wSetName wsAssurance wsUnit mnemonic spendingPassword = do
        pass <- mkCPassPhrase spendingPassword
        let cWalletInit    = mkCWalletInit wSetName wsAssurance wsUnit mnemonic
        let newCWallet     = B.newWallet tls pass

        fromAff <<< map encodeJson $ either throwError newCWallet cWalletInit

-- | Get meta information from given wallet
-- Arguments: wallet object/identifier, name, assurance, unit
-- Returns json representation of wallets within given wallet id
-- Example in nodejs:
-- | ```js
-- | > api.updateWallet('1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW', 'Initial wallet','CWANormal',0).then(console.log)
-- | Promise { <pending> }
-- | > { cwAccountsNumber: 0,
-- |   cwMeta: { cwName: 'Initial wallet', cwAssurance: 'CWANormal', cwUnit: 0 },
-- |   cwPassphraseLU: 1494583348.3572557,
-- |   cwHasPassphrase: true,
-- |   cwId: '1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW' }
-- | ```
updateWallet :: forall eff. EffFn5 (http :: HTTP, exception :: EXCEPTION | eff) TLSOptions String String String Int (Promise Json)
updateWallet = mkEffFn5 \tls wId wName wAssurance wUnit -> fromAff <<< map encodeJson <<<
    B.updateWallet tls (mkCId wId) $ mkCWalletMeta wName wAssurance wUnit

-- TODO: note that restoreWallet and newWallet are the same. They will be unified in future

-- | Restores a new wallet set.
-- Arguments: wallet set name, mnemonics, spending password (set to empty string if you don't want to set password
-- Returns json representation of restored wallet set
-- Example in nodejs:
-- | ```js
-- | >  api.restoreWallet('test', 'CWANormal', 0, 'transfer uniform grunt excess six veteran vintage warm confirm vote nephew allow', 'pass').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > Error: ServerError: Pos.Wallet.Web.Error.RequestError "Wallet set with that mnemonics already exists"
-- |
-- |
-- | >  api.deleteWallet('1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > {}
-- |
-- |
-- | >  api.restoreWallet('test', 'CWANormal', 0, 'transfer uniform grunt excess six veteran vintage warm confirm vote nephew allow', 'pass').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > { cwAccountsNumber: 0,
-- |   cwMeta: { cwUnit: 0, cwName: 'test', cwAssurance: 'CWANormal' },
-- |   cwPassphraseLU: 1495542169.630769,
-- |   cwId: '1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW',
-- |   cwHasPassphrase: true,
-- |   cwAmount: { getCCoin: '0' } }
-- | ```

-- either throwError ( $ mkCPassPhrase spendingPassword) $ mkCWalletInit wSetName wsAssurance wsUnit mnemonic

-- instance derivedErrorFromForeignError :: Error (NonEmptyList ForeignError) where

restoreWallet
    :: forall eff.
    EffFn6 (http :: HTTP, err :: EXCEPTION, exception :: EXCEPTION | eff)
    TLSOptions
    String
    String
    Int
    String
    Foreign
    (Promise Json)
restoreWallet = mkEffFn6 cRestoreWallet
  where
    cRestoreWallet
        :: TLSOptions
        -> String
        -> String
        -> Int
        -> String
        -> Foreign
        -> Eff (http :: HTTP, err :: EXCEPTION, exception :: EXCEPTION | eff) (Promise Json)
    cRestoreWallet tls wSetName wsAssurance wsUnit mnemonic spendingPassword = do
        pass <- mkCPassPhrase spendingPassword
        let cWalletInit    = mkCWalletInit wSetName wsAssurance wsUnit mnemonic
        let restoredWallet = B.restoreWallet tls pass

        fromAff <<< map encodeJson $ either throwError restoredWallet cWalletInit

-- | Rename a wallet set.
-- Arguments: wallet set id/hash, name
-- Returns json representation of renamed wallet set
-- Example in nodejs:
-- | ```js
-- | >  api.renameWalletSet('1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW', 'testing').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > { cwAccountsNumber: 0,
-- |   cwMeta: { cwUnit: 0, cwName: 'testing', cwAssurance: 'CWANormal' },
-- |   cwPassphraseLU: 1495542169.630769,
-- |   cwId: '1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW',
-- |   cwHasPassphrase: true,
-- |   cwAmount: { getCCoin: '0' } }
-- |
-- | >  api.renameWalletSet('1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW', 'test').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > { cwAccountsNumber: 0,
-- |   cwMeta: { cwUnit: 0, cwName: 'test', cwAssurance: 'CWANormal' },
-- |   cwPassphraseLU: 1495542169.630769,
-- |   cwId: '1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW',
-- |   cwHasPassphrase: true,
-- |   cwAmount: { getCCoin: '0' } }
-- | ```
renameWalletSet :: forall eff. EffFn3 (http :: HTTP, exception :: EXCEPTION | eff) TLSOptions String String (Promise Json)
renameWalletSet = mkEffFn3 \tls wSetId name -> fromAff <<< map encodeJson $ B.renameWalletSet tls (mkCId wSetId) name

-- | Import a wallet set.
-- Arguments: file path to the wallet set on a filesystem, spending password (set to empty string if you don't want to set password)
-- Returns json representation of imported wallet set
-- Example in nodejs:
-- | ```js
-- | > api.importWallet('/home/akegalj/projects/serokell/cardano-sl/keys/2.key.hd', '').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > { cwAccountsNumber: 0,
-- |   cwMeta:
-- |    { cwUnit: 0,
-- |      cwName: 'Genesis wallet set',
-- |      cwAssurance: 'CWANormal' },
-- |   cwPassphraseLU: 1495545014.377285,
-- |   cwId: '1feqWtoyaxFyvKQFWo46vHSc7urynGaRELQE62T74Y3RBs8',
-- |   cwHasPassphrase: false,
-- |   cwAmount: { getCCoin: '0' } }
-- | ```
importWallet
    :: forall eff.
    EffFn3 (http :: HTTP, err :: EXCEPTION, exception :: EXCEPTION | eff)
    TLSOptions
    String
    Foreign
    (Promise Json)
importWallet = mkEffFn3 cImportWallet
  where
    cImportWallet
        :: TLSOptions
        -> String
        -> Foreign
        -> Eff (http :: HTTP, err :: EXCEPTION, exception :: EXCEPTION | eff) (Promise Json)
    cImportWallet tls filePath spendingPassword = do
        pass <- mkCPassPhrase spendingPassword
        let importedWallet = B.importWallet tls pass (CFilePath filePath)
        fromAff <<< map encodeJson $ importedWallet

-- | Rename a wallet set.
-- Arguments: wallet set id/hash, old spending password (set to empty string if there is no password), new spending password (set to empty string if you want to remove password)
-- Returns json representation of renamed wallet set
-- Example in nodejs:
-- | ```js
-- | > api.changeWalletPass('1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW', 'pass', 'pass2').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > {}
-- | ```
changeWalletPass
    :: forall eff.
    EffFn4  (http :: HTTP, err :: EXCEPTION, exception :: EXCEPTION | eff)
    TLSOptions
    String
    Foreign
    Foreign
    (Promise Unit)
changeWalletPass = mkEffFn4 cChangeWalletPass
  where
    cChangeWalletPass
        :: TLSOptions
        -> String
        -> Foreign
        -> Foreign
        -> Eff (http :: HTTP, err :: EXCEPTION, exception :: EXCEPTION | eff) (Promise Unit)
    cChangeWalletPass tls wSetId oldPass newPass = do
        oldPass' <- mkCPassPhrase oldPass
        newPass' <- mkCPassPhrase newPass
        let walletSetId = mkCId wSetId
        fromAff $ B.changeWalletPass tls walletSetId oldPass' newPass'

-- | Deletes a wallet set.
-- Arguments: wallet set identifier
-- Returns:
-- Example in nodejs:
-- | ```js
-- | > api.deleteWallet('1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > {}
-- | ```
deleteWallet :: forall eff. EffFn2 (http :: HTTP, exception :: EXCEPTION | eff) TLSOptions String (Promise Unit)
deleteWallet = mkEffFn2 $ \tls -> fromAff <<< B.deleteWallet tls <<< mkCId

--------------------------------------------------------------------------------
-- Wallets ---------------------------------------------------------------------

-- | Get a wallet.
-- Arguments: wallet identifier
-- Returns json representation of a wallet
-- Example in nodejs:
-- | ```js
-- | > api.getAccount('1feqWtoyaxFyvKQFWo46vHSc7urynGaRELQE62T74Y3RBs8@2147483648').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > { caMeta: { caName: 'Genesis wallet' },
-- |   caId: '1feqWtoyaxFyvKQFWo46vHSc7urynGaRELQE62T74Y3RBs8@2147483648',
-- |   caAmount: { getCCoin: '50000' },
-- |   cwAddresses:
-- |    [ { cadId: '19FLnEFfkaLsZqBqYHjPmCypZNHNZ7SBfMsntKgspqA96F18s6eeDy5GYjHmwXSECG6jRqWh9qqEAicpEXrNhpb8PuRNVL',
-- |        cadAmount: [Object] } ] }
-- | ```
getAccount :: forall eff. EffFn2 (http :: HTTP, exception :: EXCEPTION | eff) TLSOptions String (Promise Json)
getAccount = mkEffFn2 $ \tls -> fromAff <<< map encodeJson <<< B.getAccount tls <<< mkCAccountId

-- | Get all wallets.
-- Arguments:
-- Returns json representation of all wallet sets
-- Example in nodejs:
-- | ```js
-- | > api.getAccounts().then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > [ { caMeta: { caName: 'Genesis wallet' },
-- |     caId: '1feqWtoyaxFyvKQFWo46vHSc7urynGaRELQE62T74Y3RBs8@2147483648',
-- |     caAmount: { getCCoin: '50000' },
-- |     cwAddresses: [ [Object] ] },
-- |   { caMeta: { caName: 'Initial wallet' },
-- |     caId: '1gCC3J43QAZo3fZiUTuyfYyT8sydFJHdhPnFFmckXL7mV3f@2147483648',
-- |     caAmount: { getCCoin: '50000' },
-- |     cwAddresses: [ [Object] ] } ]
-- | ```
getAccounts :: forall eff. EffFn1 (http :: HTTP, exception :: EXCEPTION | eff) TLSOptions (Promise Json)
getAccounts = mkEffFn1 $ fromAff <<< map encodeJson <<< flip B.getAccounts Nothing

-- | Get wallets from specific wallet set.
-- Arguments: address/hash/id of a wallet set
-- Returns json representation of wallets within given wallet set id
-- Example in nodejs:
-- | ```js
-- | > api.getWalletAccounts('1gCC3J43QAZo3fZiUTuyfYyT8sydFJHdhPnFFmckXL7mV3f').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > [ { caMeta: { caName: 'Initial wallet' },
-- |     caId: '1gCC3J43QAZo3fZiUTuyfYyT8sydFJHdhPnFFmckXL7mV3f@2147483648',
-- |     caAmount: { getCCoin: '50000' },
-- |     cwAddresses: [ [Object] ] } ]
-- | ```
getWalletAccounts :: forall eff. EffFn2 (http :: HTTP, exception :: EXCEPTION | eff) TLSOptions String (Promise Json)
getWalletAccounts = mkEffFn2 $ \tls -> fromAff <<< map encodeJson <<< B.getAccounts tls <<< Just <<< mkCId

-- | Get meta information from given account
-- Arguments: account object/identifier, name
-- Returns json representation of account with the given account id
-- Example in nodejs:
-- | ```js
-- | > api.updateAccount('1gCC3J43QAZo3fZiUTuyfYyT8sydFJHdhPnFFmckXL7mV3f@2147483648','Initial wallet').then(console.log)
-- | Promise { <pending> }
-- | > { caMeta: { caName: 'CWTPersonal' },
-- |   caId: '1gCC3J43QAZo3fZiUTuyfYyT8sydFJHdhPnFFmckXL7mV3f@2147483648',
-- |   caAmount: { getCCoin: '50000' },
-- |   cwAddresses:
-- |    [ { cadId: '19Fv6JWbdLXRXqew721u2GEarEwc8rcfpAqsriRFPameyCkQLHsNDKQRpwsM7W1M587CiswPuY27cj7RUvNXcZWgTbPByq',
-- |        cadAmount: [Object] } ] }
-- | ```
updateAccount :: forall eff. EffFn3 (http :: HTTP, exception :: EXCEPTION | eff) TLSOptions String String (Promise Json)
updateAccount = mkEffFn3 \tls wId wName -> fromAff <<< map encodeJson <<<
    B.updateAccount tls (mkCAccountId wId) $ mkCAccountMeta wName

-- | Creates a new wallet.
-- Arguments: address/hash/id of a wallet set, type, currency, name, mnemonics, spending password (if empty string is given, wallet will be created with no spending password)
-- Returns json representation of newly created wallet
-- Example in nodejs:
-- | ```js
-- | > api.newAccount('1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW', 'trips', 'pass').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > { caMeta: { caName: 'trips' },
-- |   caId: '1fjgSiJKbzJGMsHouX9HDtKai9cmvPzoTfrmYGiFjHpeDhW@3190108780',
-- |   caAmount: { getCCoin: '0' },
-- |   cwAddresses:
-- |    [ { cadId: '19M3DbeepAzN6xzSSErL8pk1JQA8oFkgE9L6LZfKXMiNpoPDjfDpJjWa3Jis1oCZVGMo1pM8tio2wifuhDPWzwCWS6sZfX',
-- |        cadAmount: [Object] } ] }
-- | ```
newAccount
    :: forall eff.
    EffFn4 (http :: HTTP, err :: EXCEPTION, exception :: EXCEPTION | eff)
    TLSOptions
    String
    String
    Foreign
    (Promise Json)
newAccount = mkEffFn4 cNewAccount
  where
    cNewAccount
        :: TLSOptions
        -> String
        -> String
        -> Foreign
        -> Eff (http :: HTTP, err :: EXCEPTION, exception :: EXCEPTION | eff) (Promise Json)
    cNewAccount tls wSetId wName spendingPassword = do
        pass <- mkCPassPhrase spendingPassword
        let accountInit = mkCAccountInit wName (mkCId wSetId)
        let newCAccount  = B.newAccount tls pass accountInit
        fromAff <<< map encodeJson $ newCAccount

-- | Deletes a wallet.
-- Arguments: wallet object/identifier
-- Returns:
-- Example in nodejs:
-- | ```js
-- | >  api.deleteAccount('1feqWtoyaxFyvKQFWo46vHSc7urynGaRELQE62T74Y3RBs8@2147483648').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > {}
-- | ```
deleteAccount :: forall eff. EffFn2 (http :: HTTP, exception :: EXCEPTION | eff) TLSOptions String (Promise Unit)
deleteAccount = mkEffFn2 $ \tls -> fromAff <<< B.deleteAccount tls <<< mkCAccountId

--------------------------------------------------------------------------------
-- Accounts ------------------------------------------------------------------

-- | Creates a new account in the wallet.
-- Arguments: wallet id, spending password
-- Returns json representation of newly created account
-- Example in nodejs:
-- | ```js
-- | > api.newWAddress('1fqJaRGbJnhyVUtrGrPs9SsyZz5fWhoC8sa8CypcKYYH565@2385431040', null).then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > { cadId: '19N52o4RrzEo6AxRzawAkbuMtnqPjrgat1USDMaRQG3uK46b7bNrpxMSLgd1sxvPUPFbGnmj9Kmj2Fb8H5W5Ez7g6voZMy',
-- |   cadAmount: { getCCoin: '0' } }
-- | ```
newWAddress
    :: forall eff.
    EffFn3 (http :: HTTP, err :: EXCEPTION, exception :: EXCEPTION | eff)
    TLSOptions
    String
    Foreign
    (Promise Json)
newWAddress = mkEffFn3 cNewWAddress
  where
    cNewWAddress
        :: TLSOptions
        -> String
        -> Foreign
        -> Eff (http :: HTTP, err :: EXCEPTION, exception :: EXCEPTION | eff) (Promise Json)
    cNewWAddress tls wId spendingPassword = do
        pass <- mkCPassPhrase spendingPassword
        let cAccountId  = mkCAccountId wId
        let newCAddress = B.newAddress tls pass cAccountId
        fromAff <<< map encodeJson $ newCAddress

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
isValidAddress :: forall eff. EffFn2 (http :: HTTP, exception :: EXCEPTION | eff) TLSOptions String (Promise Boolean)
isValidAddress = mkEffFn2 $ \tls -> fromAff <<< B.isValidAddress tls <<< mkCId

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
getLocale :: forall eff. EffFn1 (http :: HTTP, exception :: EXCEPTION | eff) TLSOptions (Promise String)
getLocale = mkEffFn1 $ \tls -> fromAff $ getProfileLocale <$> B.getProfile tls

-- | Sets user locale.
-- Arguments: new user locale
-- Returns users locale
-- Example in nodejs:
-- | ```js
-- | > api.updateLocale('en-US').then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > en-US
-- | ```
updateLocale :: forall eff. EffFn2 (http :: HTTP, exception :: EXCEPTION | eff) TLSOptions String (Promise String)
updateLocale = mkEffFn2 \tls locale -> fromAff <<< map getProfileLocale <<< B.updateProfile tls $ mkCProfile locale

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
newPayment
    :: forall eff.
    EffFn5 (http :: HTTP, err :: EXCEPTION, exception :: EXCEPTION | eff)
    TLSOptions
    String
    String
    String
    Foreign
    (Promise Json)
newPayment = mkEffFn5 cNewPayment
  where
    cNewPayment
        :: TLSOptions
        -> String
        -> String
        -> String
        -> Foreign
        -> Eff  (http :: HTTP, err :: EXCEPTION, exception :: EXCEPTION | eff) (Promise Json)
    cNewPayment tls wFrom addrTo amount spendingPassword = do
        pass <- mkCPassPhrase spendingPassword
        let accountId   = mkCAccountId wFrom
        let cId         = mkCId addrTo
        let cAmount     = mkCCoin amount
        let newCPayment = B.newPayment tls pass accountId cId cAmount Nothing

        fromAff <<< map encodeJson $ newCPayment

-- TODO: add documentation
-- This is similar to newPayment, except it returns how much fees would the payment take
txFee
    :: forall eff.
    EffFn4 (http :: HTTP, exception :: EXCEPTION | eff)
    TLSOptions
    String
    String
    String
    (Promise Json)
txFee = mkEffFn4 cTxFee
  where
    cTxFee
        :: TLSOptions
        -> String
        -> String
        -> String
        -> Eff  (http :: HTTP, exception :: EXCEPTION | eff) (Promise Json)
    cTxFee tls wFrom addrTo amount = do
        let accountId   = mkCAccountId wFrom
        let cId         = mkCId addrTo
        let cAmount     = mkCCoin amount
        let txFee'      = B.txFee tls accountId cId cAmount Nothing

        fromAff <<< map encodeJson $ txFee'


-- | Updates transaction meta data.
-- Arguments: wallet object/id, transaction id/hash, currency, title, description, date
-- Returns
-- Example in nodejs:
-- | ```js
-- | > api.updateTransaction('1gCC3J43QAZo3fZiUTuyfYyT8sydFJHdhPnFFmckXL7mV3f@2147483648', 'cc7576fef33a4a60865f9149792fa7359f44eca6745aeb1ba751185bab9bd7ac', 'ADA', 'Manager task', 'Managing people and other stuff', 1494935150.0468155).then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > {}
-- | ```
updateTransaction :: forall eff. EffFn4 (http :: HTTP, exception :: EXCEPTION | eff) TLSOptions String String Number (Promise Unit)
updateTransaction = mkEffFn4 \tls wId ctxId ctmDate -> fromAff $
    B.updateTransaction
    tls
    (mkCAccountId wId)
    (mkCTxId ctxId)
    (mkCTxMeta ctmDate)

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

getHistory :: forall eff. EffFn6 (http :: HTTP, err :: EXCEPTION, exception :: EXCEPTION | eff) TLSOptions Foreign Foreign Foreign Int Int (Promise Json)
getHistory = mkEffFn6 \tls wIdF acIdF addressIdF skip limit -> do
    wId <- optionalString wIdF "walletId"
    acId <- optionalString acIdF "accountId"
    addressId <- optionalString addressIdF "addressId"
    fromAff <<< map encodeJson $ do
        B.getHistory
            tls
            (mkCId <$> wId)
            (mkCAccountId <$> acId)
            (mkCId <$> addressId)
            (Just skip)
            (Just limit)

getHistoryByAccount :: forall eff. EffFn4 (http :: HTTP, exception :: EXCEPTION | eff) TLSOptions String Int Int (Promise Json)
getHistoryByAccount = mkEffFn4 \tls acId skip limit -> fromAff <<< map encodeJson $
    B.getHistory
    tls
    Nothing
    (Just $ mkCAccountId acId)
    Nothing
    (Just skip)
    (Just limit)

-- TODO: this is a workaround https://issues.serokell.io/issue/CSM-300
getHistoryByWallet :: forall eff. EffFn4 (http :: HTTP, exception :: EXCEPTION | eff) TLSOptions String Int Int (Promise Json)
getHistoryByWallet = mkEffFn4 \tls wId skip limit -> fromAff <<< map encodeJson $
    B.getHistory
    tls
    (Just $ mkCId wId)
    Nothing
    Nothing
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
getAddressHistory :: forall eff. EffFn5 (http :: HTTP, exception :: EXCEPTION | eff) TLSOptions String String Int Int (Promise Json)
getAddressHistory = mkEffFn5 \tls acId address skip limit -> fromAff <<< map encodeJson $
    B.getHistory
    tls
    Nothing
    (Just $ mkCAccountId acId)
    (Just $ mkCId address)
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
nextUpdate :: forall eff. EffFn1 (http :: HTTP, exception :: EXCEPTION | eff) TLSOptions (Promise Json)
nextUpdate = mkEffFn1 $ fromAff <<< map encodeJson <<< B.nextUpdate

-- Example in nodejs:
-- | ```js
-- | > api.postponeUpdate().then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > {}
-- | ```
postponeUpdate :: forall eff. EffFn1 (http :: HTTP, exception :: EXCEPTION | eff) TLSOptions (Promise Unit)
postponeUpdate = mkEffFn1 $ fromAff <<< B.postponeUpdate

-- Example in nodejs:
-- | ```js
-- | > api.applyUpdate().then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > {}
-- | ```
applyUpdate :: forall eff. EffFn1 (http :: HTTP, exception :: EXCEPTION | eff) TLSOptions (Promise Unit)
applyUpdate = mkEffFn1 $ fromAff <<< B.applyUpdate

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
redeemAda
    :: forall eff.
    EffFn4 (http :: HTTP, err :: EXCEPTION, exception :: EXCEPTION, crypto :: Crypto.CRYPTO | eff)
    TLSOptions
    String
    String
    Foreign
    (Promise Json)
redeemAda = mkEffFn4 cRedeemAda
  where
    cRedeemAda
        :: TLSOptions
        -> String
        -> String
        -> Foreign
        -> Eff (http :: HTTP, err :: EXCEPTION, exception :: EXCEPTION, crypto :: Crypto.CRYPTO | eff) (Promise Json)
    cRedeemAda tls seed wId spendingPassword = do
        pass <- mkCPassPhrase spendingPassword
        let accountId    = mkCAccountId wId
        let walletRedeem = mkCWalletRedeem seed accountId
        let redeemedAda  = B.redeemAda tls pass walletRedeem

        fromAff <<< map encodeJson $ redeemedAda

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
redeemAdaPaperVend
    :: forall eff.
    EffFn5 (http :: HTTP, err :: EXCEPTION, exception :: EXCEPTION, crypto :: Crypto.CRYPTO | eff)
    TLSOptions
    String
    String
    String
    Foreign
    (Promise Json)
redeemAdaPaperVend = mkEffFn5 cRedeemAdaPaperVend
  where
    cRedeemAdaPaperVend
        :: TLSOptions
        -> String
        -> String
        -> String
        -> Foreign
        -> Eff (http :: HTTP, err :: EXCEPTION, exception :: EXCEPTION, crypto :: Crypto.CRYPTO | eff) (Promise Json)
    cRedeemAdaPaperVend tls seed mnemonic wId spendingPassword = do
        pass <- mkCPassPhrase spendingPassword
        let accountId    = mkCAccountId wId
        let walletRedeem = mkCPaperVendWalletRedeem seed mnemonic accountId
        let redeemedAda  = B.redeemAdaPaperVend tls pass

        fromAff <<< map encodeJson $ either throwError redeemedAda walletRedeem

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
reportInit :: forall eff. EffFn3 (http :: HTTP, exception :: EXCEPTION, crypto :: Crypto.CRYPTO | eff) TLSOptions Int Int (Promise Unit)
reportInit = mkEffFn3 \tls total -> fromAff <<< B.reportInit tls <<< mkCInitialized total

--------------------------------------------------------------------------------
-- Settings ---------------------------------------------------------------------

-- Example in nodejs:
-- | ```js
-- | > api.blockchainSlotDuration().then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > 7000
-- | ```
blockchainSlotDuration :: forall eff. EffFn1 (http :: HTTP, exception :: EXCEPTION | eff) TLSOptions (Promise Int)
blockchainSlotDuration = mkEffFn1 $ fromAff <<< B.blockchainSlotDuration

-- Example in nodejs:
-- | ```js
-- | > api.systemVersion().then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > { svNumber: 0, svAppName: { getApplicationName: 'cardano-sl' } }
-- | ```
systemVersion :: forall eff. EffFn1 (http :: HTTP, exception :: EXCEPTION | eff) TLSOptions (Promise Json)
systemVersion = mkEffFn1 $ fromAff <<< map encodeJson <<< B.systemVersion

-- Example in nodejs:
-- | ```js
-- | > api.syncProgress().then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > { _spPeers: 0,
-- |   _spNetworkCD: null,
-- |   _spLocalCD: { getChainDifficulty: 4 } }
-- | ```
syncProgress :: forall eff. EffFn1 (http :: HTTP, exception :: EXCEPTION | eff) TLSOptions (Promise Json)
syncProgress = mkEffFn1 $ fromAff <<< map encodeJson <<< B.syncProgress

-- Example in nodejs:
-- | ```js
-- | > api.localTimeDifference().then(console.log).catch(console.log)
-- | Promise { <pending> }
-- | > 0
-- | ```
localTimeDifference :: forall eff. EffFn1 (http :: HTTP, exception :: EXCEPTION | eff) TLSOptions (Promise Json)
localTimeDifference = mkEffFn1 $ fromAff <<< map encodeJson <<< B.localTimeDifference

--------------------------------------------------------------------------------
-- JSON backup -----------------------------------------------------------------
importBackupJSON :: forall eff. EffFn2 (http :: HTTP, exception :: EXCEPTION | eff) TLSOptions String (Promise Json)
importBackupJSON = mkEffFn2 $ \tls -> fromAff <<< map encodeJson <<< B.importBackupJSON tls <<< CFilePath

exportBackupJSON :: forall eff. EffFn3 (http :: HTTP, exception :: EXCEPTION | eff) TLSOptions String String (Promise Unit)
exportBackupJSON = mkEffFn3 $ \tls wId path -> fromAff $ B.exportBackupJSON tls (mkCId wId) (CFilePath path)

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
