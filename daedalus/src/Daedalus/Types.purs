module Daedalus.Types
       ( module CT
       , module C
       , module E
       , module BP
       , module DT
       , _address
       , _ccoin
       , _passPhrase
       , mkCCoin
       , mkCAddress
       , mkCWalletMeta
       , mkCWalletInit
       , mkCTxMeta
       , mkCTxId
       , mkCCurrency
       , mkCProfile
       , _ctxIdValue
       , showCCurrency
       , mkBackupPhrase
       , mkCWalletRedeem
       , mkCPaperVendWalletRedeem
       , mkCInitialized
       , mkCPassPhrase
       , emptyCPassPhrase
       , getProfileLocale
       , walletAddressToUrl
       , mkCWalletSetInit
       ) where

import Prelude

import Pos.Wallet.Web.ClientTypes (CAddress (..), CHash (..), CPassPhrase (..), CCoin (..), WS (..), CWalletAddress (..), CWalletSetMeta (..))

import Pos.Wallet.Web.ClientTypes as CT
import Pos.Core.Types as C
import Pos.Wallet.Web.Error as E
import Pos.Util.BackupPhrase (BackupPhrase (..))
import Pos.Util.BackupPhrase as BP

import Control.Monad.Eff.Exception (error, Error)
import Data.Either (either, Either (..))
import Data.Maybe (Maybe (..))
import Data.Argonaut.Generic.Aeson (decodeJson)
import Data.Argonaut.Core (fromString)
import Data.Generic (gShow)
import Data.Array.Partial (last)
import Data.Array (length, filter)
import Partial.Unsafe (unsafePartial)
import Data.String (split, null, trim, joinWith, Pattern (..))

import Daedalus.Crypto (isValidMnemonic, blake2b, bytesToB16)
import Data.Types (mkTime)
import Data.Types as DT
import Data.Int53 (fromInt, toString)

space :: Pattern
space = Pattern " "

dot :: Pattern
dot = Pattern "."

backupMnemonicLen :: Int
backupMnemonicLen = 12

paperVendMnemonicLen :: Int
paperVendMnemonicLen = 9

mkBackupPhrase :: Int -> String -> Either Error BackupPhrase
mkBackupPhrase len mnemonic = mkBackupPhraseIgnoreChecksum len mnemonic >>= const do
    if not $ isValidMnemonic mnemonicCleaned
        then Left $ error "Invalid mnemonic: checksum missmatch"
        else Right $ BackupPhrase { bpToList: split space mnemonicCleaned }
  where
    mnemonicCleaned = cleanMnemonic mnemonic

cleanMnemonic :: String -> String
cleanMnemonic = joinWith " " <<< filter (not <<< null) <<< split space <<< trim

mkBackupPhraseIgnoreChecksum :: Int -> String -> Either Error BackupPhrase
mkBackupPhraseIgnoreChecksum len mnemonic =
    if not $ hasExactlyNwords len mnemonicCleaned
        then Left $ error $ "Invalid mnemonic: mnemonic should have at least " <> show len <> " words"
        else Right $ BackupPhrase { bpToList: split space mnemonicCleaned }
  where
    hasExactlyNwords len' = (==) len' <<< length <<< split space
    mnemonicCleaned = cleanMnemonic mnemonic

showCCurrency :: CT.CCurrency -> String
showCCurrency = dropModuleName <<< gShow
  where
    -- TODO: this is again stupid. We should derive Show for this type instead of doing this
    dropModuleName = unsafePartial last <<< split dot

-- TODO: it would be useful to extend purescript-bridge
-- and generate lenses
walletAddressToUrl :: CWalletAddress -> String
walletAddressToUrl (CWalletAddress r) = _address r.cwaWSAddress <> "@" <> toString r.cwaIndex

_hash :: CHash -> String
_hash (CHash h) = h

_address :: forall a. CAddress a -> String
_address (CAddress a) = _hash a

_passPhrase :: CPassPhrase -> String
_passPhrase (CPassPhrase p) = p

emptyCPassPhrase :: CPassPhrase
emptyCPassPhrase = CPassPhrase ""

mkCPassPhrase :: String -> Maybe CPassPhrase
mkCPassPhrase "" = Nothing
mkCPassPhrase pass = Just <<< CPassPhrase <<< bytesToB16 $ blake2b pass

mkCAddress :: forall a. String -> CAddress a
mkCAddress = CAddress <<< CHash

_ccoin :: CCoin -> String
_ccoin (CCoin c) = c.getCoin

mkCCoin :: String -> CCoin
mkCCoin amount = CCoin { getCoin: amount }

-- NOTE: use genericRead maybe https://github.com/paluh/purescript-generic-read-example
mkCWalletType :: String -> CT.CWalletType
mkCWalletType = either (const CT.CWTPersonal) id <<< decodeJson <<< fromString

mkCCurrency :: String -> CT.CCurrency
mkCCurrency = either (const CT.ADA) id <<< decodeJson <<< fromString

mkCWAssurance :: String -> CT.CWalletAssurance
mkCWAssurance = either (const CT.CWANormal) id <<< decodeJson <<< fromString

mkCWalletMeta :: String -> String -> String -> String -> Int -> CT.CWalletMeta
mkCWalletMeta wType wCurrency wName wAssurance wUnit =
    CT.CWalletMeta { cwType: mkCWalletType wType
                   , cwCurrency: mkCCurrency wCurrency
                   , cwName: wName
                   , cwAssurance: mkCWAssurance wAssurance
                   , cwUnit: wUnit
                   }

mkCInitialized :: Int -> Int -> CT.CInitialized
mkCInitialized total preInit =
    CT.CInitialized { cTotalTime: fromInt total
                    , cPreInit: fromInt preInit
                    }

mkCWalletInit :: String -> String -> String -> CAddress WS -> CT.CWalletInit
mkCWalletInit wType wCurrency wName wSetId =
    CT.CWalletInit { cwInitWSetId: wSetId
                   , cwInitMeta: mkCWalletMeta wType wCurrency wName "CWANormal" 0 -- FIXME: don't use string!
                   }

mkCWalletSetInit :: String -> String -> Either Error CT.CWalletSetInit
mkCWalletSetInit wSetName mnemonic = do
    bp <- mkBackupPhrase backupMnemonicLen mnemonic
    pure $ CT.CWalletSetInit { cwsInitMeta: CWalletSetMeta {cwsName: wSetName }
                             , cwsBackupPhrase: bp
                             }


mkCWalletRedeem :: String -> CWalletAddress -> CT.CWalletRedeem
mkCWalletRedeem seed wAddress = do
    CT.CWalletRedeem { crWalletId: wAddress
                     , crSeed: seed
                     }

mkCPaperVendWalletRedeem :: String -> String -> CWalletAddress -> Either Error CT.CPaperVendWalletRedeem
mkCPaperVendWalletRedeem seed mnemonic wAddress = do
    bp <- mkBackupPhrase paperVendMnemonicLen mnemonic
    pure $ CT.CPaperVendWalletRedeem { pvWalletId: wAddress
                                     , pvBackupPhrase: bp
                                     , pvSeed: seed
                                     }

_ctxIdValue :: CT.CTxId -> String
_ctxIdValue (CT.CTxId tx) = _hash tx

mkCTxId :: String -> CT.CTxId
mkCTxId = CT.CTxId <<< CHash

mkCTxMeta :: String -> String -> String -> Number -> CT.CTxMeta
mkCTxMeta currency title description date =
    CT.CTxMeta { ctmCurrency: mkCCurrency currency
               , ctmTitle: title
               , ctmDescription: description
               , ctmDate: mkTime date
               }

mkCProfile :: String -> CT.CProfile
mkCProfile locale =
    CT.CProfile { cpLocale: locale
                }

getProfileLocale :: CT.CProfile -> String
getProfileLocale (CT.CProfile r) = r.cpLocale
