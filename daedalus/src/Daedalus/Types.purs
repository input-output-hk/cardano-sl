module Daedalus.Types
       ( module CT
       , module C
       , module E
       , module BP
       , module DT
       , module CLI
       , _address
       , _ccoin
       , _passPhrase
       , mkCCoin
       , mkCId
       , mkCAccountMeta
       , mkCAccountInit
       , mkCAccountId
       , mkCTxMeta
       , mkCTxId
       , mkCProfile
       , _ctxIdValue
       , mkBackupPhrase
       , mkCWalletRedeem
       , mkCWalletMeta
       , mkCPaperVendWalletRedeem
       , mkCInitialized
       , mkCPassPhrase
       , emptyCPassPhrase
       , getProfileLocale
       , walletAddressToUrl
       , mkCWalletInit
       , mkCWalletAssurance
       , optionalString
       ) where

import Prelude
import Data.Types as DT
import Pos.Core.Common.Types as C
import Pos.Core.Update.Types as C
import Pos.Client.Txp.Util as CLI
import Pos.Util.BackupPhrase as BP
import Pos.Wallet.Web.ClientTypes.Types as CT
import Pos.Wallet.Web.Error.Types as E
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, Error, error, throw)
import Control.Monad.Except (runExcept)
import Daedalus.Crypto (isValidMnemonic, blake2b, bytesToB16)
import Data.Argonaut.Core (fromString)
import Data.Argonaut.Generic.Aeson (decodeJson)
import Data.Array (length, filter)
import Data.Either (either, Either(..))
import Data.Foreign (F, Foreign, isNull, readString)
import Data.Int53 (fromInt)
import Data.Maybe (Maybe(..))
import Data.String (split, null, trim, joinWith, Pattern(..))
import Data.Types (mkTime)
import Pos.Util.BackupPhrase (BackupPhrase(..))
import Pos.Wallet.Web.ClientTypes.Types (CId(..), CHash(..), CPassPhrase(..), CCoin(..), Wal, CAccountId(..), CWalletMeta(..))

space :: Pattern
space = Pattern " "

dot :: Pattern
dot = Pattern "."

backupMnemonicLen :: Int
backupMnemonicLen = 12

paperVendMnemonicLen :: Int
paperVendMnemonicLen = 9

-- NOTE: if you will be bumping bip39 to >=2.2.0 be aware of https://issues.serokell.io/issue/VD-95 . In this case you will have to modify how we validate paperVendMnemonics.
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
        then Left $ error $ "Invalid mnemonic: mnemonic should have exactly " <> show len <> " words"
        else Right $ BackupPhrase { bpToList: split space mnemonicCleaned }
  where
    hasExactlyNwords len' = (==) len' <<< length <<< split space
    mnemonicCleaned = cleanMnemonic mnemonic

-- TODO: it would be useful to extend purescript-bridge
-- and generate lenses
walletAddressToUrl :: CAccountId -> String
walletAddressToUrl (CAccountId r) = r

_hash :: CHash -> String
_hash (CHash h) = h

_address :: forall a. CId a -> String
_address (CId a) = _hash a

_passPhrase :: CPassPhrase -> String
_passPhrase (CPassPhrase p) = p

emptyCPassPhrase :: CPassPhrase
emptyCPassPhrase = CPassPhrase ""

-- | Create/Make password from foreign javascript code. Return errors if you
-- find them.
mkCPassPhrase :: forall eff. Foreign -> Eff (exception :: EXCEPTION | eff) (Maybe CPassPhrase)
mkCPassPhrase pass = do
  optionalPass <- optionalString pass "password"
  pure $ CPassPhrase <<< bytesToB16 <<< blake2b <$> optionalPass

-- | We take a @Foreign@ parameter and if it's null, we return it as @Nothing@,
-- and if it's not null, we try to parse it as a @String@, and return errors
-- associated with converting.
optionalString :: forall eff. Foreign -> String -> Eff (exception :: EXCEPTION | eff) (Maybe String)
optionalString string paramName =
    if (isNull string)
    then pure Nothing
    else either (const raiseError) pure $ runExcept theReadString
  where
    theReadString :: F (Maybe String)
    theReadString
        | isNull string = pure Nothing
        | otherwise = map Just $ readString string

    raiseError = throw ("Error with converting parameter '" <> paramName <> "' to string.")

mkCId :: forall a. String -> CId a
mkCId = CId <<< CHash

_ccoin :: CCoin -> String
_ccoin (CCoin c) = c.getCCoin

mkCCoin :: String -> CCoin
mkCCoin amount = CCoin { getCCoin: amount }

-- NOTE: use genericRead maybe https://github.com/paluh/purescript-generic-read-example
mkCWSetAssurance :: String -> CT.CWalletAssurance
mkCWSetAssurance = either (const CT.CWANormal) id <<< decodeJson <<< fromString

mkCAccountMeta :: String -> CT.CAccountMeta
mkCAccountMeta wName =
    CT.CAccountMeta { caName: wName
                   }

mkCAccountId :: String -> CT.CAccountId
mkCAccountId = CAccountId

mkCInitialized :: Int -> Int -> CT.CInitialized
mkCInitialized total preInit =
    CT.CInitialized { cTotalTime: fromInt total
                    , cPreInit: fromInt preInit
                    }

mkCAccountInit :: String -> CId Wal -> CT.CAccountInit
mkCAccountInit wName wSetId =
    CT.CAccountInit { caInitWId: wSetId
                    , caInitMeta: mkCAccountMeta wName
                    }

mkCWalletAssurance :: String -> CT.CWalletAssurance
mkCWalletAssurance = either (const CT.CWANormal) id <<< decodeJson <<< fromString

mkCWalletMeta :: String -> String -> Int -> CWalletMeta
mkCWalletMeta wName wAssurance wUnit =
  CWalletMeta { cwName: wName
                 , cwAssurance: mkCWalletAssurance wAssurance
                 , cwUnit: wUnit
                 }

mkCWalletInit :: String -> String -> Int -> String -> Either Error CT.CWalletInit
mkCWalletInit wSetName wsAssurance wsUnit mnemonic = do
    bp <- mkBackupPhrase backupMnemonicLen mnemonic
    pure $ CT.CWalletInit { cwInitMeta:
                                CWalletMeta
                                    { cwName: wSetName
                                    , cwAssurance: mkCWalletAssurance wsAssurance
                                    , cwUnit: wsUnit
                                    }
                             , cwBackupPhrase: bp
                             }


mkCWalletRedeem :: String -> CAccountId -> CT.CWalletRedeem
mkCWalletRedeem seed wAddress = do
    CT.CWalletRedeem { crWalletId: wAddress
                     , crSeed: seed
                     }

-- NOTE: if you will be bumping bip39 to >=2.2.0 be aware of https://issues.serokell.io/issue/VD-95 . In this case you will have to modify how we validate paperVendMnemonics.
mkCPaperVendWalletRedeem :: String -> String -> CAccountId -> Either Error CT.CPaperVendWalletRedeem
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

mkCTxMeta :: Number -> CT.CTxMeta
mkCTxMeta date =
    CT.CTxMeta { ctmDate: mkTime date
               }

mkCProfile :: String -> CT.CProfile
mkCProfile locale =
    CT.CProfile { cpLocale: locale
                }

getProfileLocale :: CT.CProfile -> String
getProfileLocale (CT.CProfile r) = r.cpLocale
