module Daedalus.Types
       ( module CT
       , module C
       , module E
       , module BP
       , module DT
       , _address
       , _coin
       , _passPhrase
       , mkCoin
       , mkCAddress
       , mkCWalletMeta
       , mkCWalletInit
       , mkCWalletInitIgnoreChecksum
       , mkCTxMeta
       , mkCTxId
       , mkCCurrency
       , mkCProfile
       , _ctxIdValue
       , showCCurrency
       , mkBackupPhrase
       , mkCWalletRedeem
       , mkCPostVendWalletRedeem
       , mkCInitialized
       , mkCPassPhrase
       , getProfileLocale
       ) where

import Prelude

import Pos.Wallet.Web.ClientTypes (CAddress (..), CHash (..), CPassPhrase (..))
import Pos.Core.Types (Coin (..))

import Pos.Wallet.Web.ClientTypes as CT
import Pos.Core.Types as C
import Pos.Wallet.Web.Error as E
import Pos.Util.BackupPhrase (BackupPhrase (..))
import Pos.Util.BackupPhrase as BP

import Control.Monad.Eff.Exception (error, Error)
import Control.Apply ((<$>))
import Data.Either (either, Either (..))
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

space :: Pattern
space = Pattern " "

dot :: Pattern
dot = Pattern "."

mkBackupPhrase :: String -> Either Error BackupPhrase
mkBackupPhrase mnemonic = mkBackupPhraseIgnoreChecksum mnemonic >>= const do
    if not $ isValidMnemonic mnemonicCleaned
        then Left $ error "Invalid mnemonic: checksum missmatch"
        else Right $ BackupPhrase { bpToList: split space mnemonicCleaned }
  where
    mnemonicCleaned = cleanMnemonic mnemonic

cleanMnemonic :: String -> String
cleanMnemonic = joinWith " " <<< filter (not <<< null) <<< split space <<< trim

mkBackupPhraseIgnoreChecksum :: String -> Either Error BackupPhrase
mkBackupPhraseIgnoreChecksum mnemonic =
    if not $ hasAtLeast12words mnemonicCleaned
        then Left $ error "Invalid mnemonic: mnemonic should have at least 12 words"
        else Right $ BackupPhrase { bpToList: split space mnemonicCleaned }
  where
    hasAtLeast12words = (<=) 12 <<< length <<< split space
    mnemonicCleaned = cleanMnemonic mnemonic

showCCurrency :: CT.CCurrency -> String
showCCurrency = dropModuleName <<< gShow
  where
    -- TODO: this is again stupid. We should derive Show for this type instead of doing this
    dropModuleName = unsafePartial last <<< split dot

-- TODO: it would be useful to extend purescript-bridge
-- and generate lenses

_hash :: CHash -> String
_hash (CHash h) = h

_address :: CAddress -> String
_address (CAddress a) = _hash a

_passPhrase :: CPassPhrase -> String
_passPhrase (CPassPhrase p) = p

mkCPassPhrase :: String -> CPassPhrase
mkCPassPhrase = CPassPhrase <<< bytesToB16 <<< blake2b

mkCAddress :: String -> CAddress
mkCAddress = CAddress <<< CHash

_coin :: Coin -> Int
_coin (Coin c) = c.getCoin

mkCoin :: Int -> Coin
mkCoin amount = Coin { getCoin: amount }

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
    CT.CInitialized { cTotalTime: total
                    , cPreInit: preInit
                    }

mkCWalletInit :: String -> String -> String -> String -> Either Error CT.CWalletInit
mkCWalletInit wType wCurrency wName mnemonic =
    mkCWalletInit' wType wCurrency wName <$> mkBackupPhrase mnemonic

mkCWalletInitIgnoreChecksum :: String -> String -> String -> String -> Either Error CT.CWalletInit
mkCWalletInitIgnoreChecksum wType wCurrency wName mnemonic= do
    mkCWalletInit' wType wCurrency wName <$> mkBackupPhraseIgnoreChecksum mnemonic

mkCWalletInit' :: String -> String -> String -> BackupPhrase -> CT.CWalletInit
mkCWalletInit' wType wCurrency wName bp =
    CT.CWalletInit { cwBackupPhrase: bp
                   , cwInitMeta: mkCWalletMeta wType wCurrency wName "CWANormal" 0 -- FIXME: don't use string!
                   }

mkCWalletRedeem :: String -> String -> CT.CWalletRedeem
mkCWalletRedeem seed wId = do
    CT.CWalletRedeem { crWalletId: mkCAddress wId
                     , crSeed: seed
                     }

mkCPostVendWalletRedeem :: String -> String -> String -> Either Error CT.CPostVendWalletRedeem
mkCPostVendWalletRedeem seed mnemonic wId = do
    bp <- mkBackupPhrase mnemonic
    pure $ CT.CPostVendWalletRedeem { pvWalletId: mkCAddress wId
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
