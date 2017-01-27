module Daedalus.Types
       ( module CT
       , module C
       , module E
       , module BP
       , _address
       , _coin
       , mkCoin
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
       , Seed
       , mkSeed
       ) where

import Prelude

import Pos.Wallet.Web.ClientTypes (CAddress (..), CHash (..))
import Pos.Types.Core (Coin (..))

import Pos.Wallet.Web.ClientTypes as CT
import Pos.Types.Core as C
import Pos.Wallet.Web.Error as E
import Pos.Util.BackupPhrase (BackupPhrase (..))
import Pos.Util.BackupPhrase as BP

import Control.Monad.Eff.Exception (error, Error)
import Data.Either (either, Either (..))
import Data.Argonaut.Generic.Aeson (decodeJson)
import Data.Argonaut.Core (fromString)
import Data.Generic (gShow)
import Data.Array.Partial (last)
import Partial.Unsafe (unsafePartial)
import Data.String (split)

import Daedalus.Crypto (isValidMnemonic)
import Data.Types (mkTime)

type Seed = String

mkSeed :: String -> Seed
mkSeed = id

mkBackupPhrase :: String -> Either Error BackupPhrase
mkBackupPhrase mnemonic =
    if not $ isValidMnemonic mnemonic
        then Left $ error "Invalid mnemonic"
        else Right $ BackupPhrase { bpToList: split " " mnemonic }

showCCurrency :: CT.CCurrency -> String
showCCurrency = dropModuleName <<< gShow
  where
    -- TODO: this is again stupid. We should derive Show for this type instead of doing this
    dropModuleName = unsafePartial last <<< split "."

-- TODO: it would be useful to extend purescript-bridge
-- and generate lenses

_hash :: CHash -> String
_hash (CHash h) = h

_address :: CAddress -> String
_address (CAddress a) = _hash a

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

mkCWalletMeta :: String -> String -> String -> CT.CWalletMeta
mkCWalletMeta wType wCurrency wName =
    CT.CWalletMeta { cwType: mkCWalletType wType
                   , cwCurrency: mkCCurrency wCurrency
                   , cwName: wName
                   }

mkCWalletInit :: String -> String -> String -> String -> Either Error CT.CWalletInit
mkCWalletInit wType wCurrency wName mnemonic = do
    bp <- mkBackupPhrase mnemonic
    pure $ CT.CWalletInit { cwBackupPhrase: bp
                          , cwInitMeta: mkCWalletMeta wType wCurrency wName
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

mkCProfile :: String -> String -> String -> String -> Number -> String -> String -> CT.CProfile
mkCProfile name email phone pass date locale picture =
    CT.CProfile { cpName: name
                , cpEmail: email
                , cpPhoneNumber: phone
                , cpPwHash: pass
                , cpPwCreated: mkTime date
                , cpLocale: locale
                , cpPicture: picture
                }
