{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Documentation of wallet web API.

module Pos.Wallet.Web.Doc
       ( walletDocsText
       ) where

import           Control.Lens               ((.~), (<>~))
import qualified Data.HashMap.Strict        as HM
import           Data.Proxy                 (Proxy (..))
import           Network.HTTP.Types.Method  (methodPost)
import           Servant.API                (Capture)
import           Servant.Docs               (API, DocCapture (..), DocIntro (..),
                                             DocNote (..), ExtraInfo (..),
                                             ToCapture (toCapture), ToSample (toSamples),
                                             defAction, defEndpoint, defaultDocOptions,
                                             docsWith, markdown, method, notes, path,
                                             singleSample)
import qualified Servant.Docs               as SD
import           System.IO.Unsafe           (unsafePerformIO)
import           Universum

import           Pos.Aeson.ClientTypes      ()
import           Pos.Crypto                 (keyGen)
import           Pos.Types                  (Coin, makePubKeyAddress, mkCoin)
import           Pos.Wallet.Web.Api         (walletApi)
import           Pos.Wallet.Web.ClientTypes (CAddress, CCurrency, CHash, CTx, CTxId,
                                             CTxMeta, CWallet, CWalletMeta,
                                             addressToCAddress)
import           Pos.Wallet.Web.Error       (WalletError)

walletDocs :: API
walletDocs = docsWith defaultDocOptions intros extras (SD.pretty walletApi)

walletDocsText :: Text
walletDocsText = toText $ markdown walletDocs

intros :: [DocIntro]
intros =
    [ DocIntro
          "Documentation of cardano-wallet web API"
          ["This is very first version, don't expect it to be smart."]
    ]

-- [CSL-234]: this is unsafe solution, but I didn't manage to make
-- safe one work :(
extras :: ExtraInfo api
extras =
    ExtraInfo . HM.fromList $
    [ (defEndpoint & path <>~ ["addresses"], defAction & notes <>~ addressesNotes)
    , ( defEndpoint & path <>~ ["send"] & method .~ methodPost
      , defAction & notes <>~ sendNotes)
    ]
  where
    addressesNotes =
        [ DocNote
          { _noteTitle = "Description"
          , _noteBody = ["Returns all addresses contained in wallet."]
          }
        ]
    sendNotes =
        [ DocNote
          { _noteTitle = "Description"
          , _noteBody =
              ["Send coins from one address from wallet to arbitrary address"]
          }
        ]

----------------------------------------------------------------------------
-- Orphan instances
----------------------------------------------------------------------------

instance ToCapture (Capture "from" CAddress) where
    toCapture Proxy =
        DocCapture
        { _capSymbol = "from"
        , _capDesc = "Address from which coins should be sent."
        }

instance ToCapture (Capture "to" CAddress) where
    toCapture Proxy =
        DocCapture
        { _capSymbol = "to"
        , _capDesc = "Destination address."
        }

instance ToCapture (Capture "amount" Coin) where
    toCapture Proxy =
        DocCapture
        { _capSymbol = "amount"
        , _capDesc = "Amount of coins to send."
        }

instance ToCapture (Capture "address" CAddress) where
    toCapture Proxy =
        DocCapture
        { _capSymbol = "address"
        , _capDesc = "Address, history of which should be fetched"
        }

instance ToCapture (Capture "index" Word) where
    toCapture Proxy =
        DocCapture
        { _capSymbol = "index"
        , _capDesc = "Index of address to delete"
        }

instance ToCapture (Capture "transaction" CTxId) where
    toCapture Proxy =
        DocCapture
        { _capSymbol = "transaction"
        , _capDesc = "Transaction id"
        }

instance ToCapture (Capture "address" Text) where
    toCapture Proxy =
        DocCapture
        { _capSymbol = "address"
        , _capDesc = "Address"
        }

instance ToCapture (Capture "description" Text) where
    toCapture Proxy =
        DocCapture
        { _capSymbol = "description"
        , _capDesc = "Transaction description"
        }

instance ToCapture (Capture "title" Text) where
    toCapture Proxy =
        DocCapture
        { _capSymbol = "title"
        , _capDesc = "Transaction title"
        }

instance ToCapture (Capture "search" Text) where
    toCapture Proxy =
        DocCapture
        { _capSymbol = "search"
        , _capDesc = "Wallet title search pattern"
        }

instance ToCapture (Capture "limit" Word) where
    toCapture Proxy =
        DocCapture
        { _capSymbol = "limit"
        , _capDesc = "Max numbers of wallets to return"
        }

instance ToCapture (Capture "currency" CCurrency) where
    toCapture Proxy =
        DocCapture
        { _capSymbol = "currency"
        , _capDesc = "Currency"
        }

instance ToSample WalletError where
    toSamples Proxy = fail "ToSample WalletError: Not Implemented!"

instance ToSample Coin where
    toSamples Proxy = singleSample (mkCoin 100500)

-- instance ToSample Address where
--     toSamples Proxy = singleSample $ genesisAddresses !! 0
--
-- FIXME!
instance ToSample CHash where
    toSamples Proxy = fail "ToSample CHash: Not Implemented!"

-- FIXME!
instance ToSample CWallet where
    toSamples Proxy = fail "ToSample CWallet: Not Implemented!"

-- FIXME!
instance ToSample CWalletMeta where
    toSamples Proxy = fail "ToSample CWalletMeta: Not Implemented!"

instance ToSample CAddress where
    toSamples Proxy = singleSample . addressToCAddress . makePubKeyAddress . fst $
        unsafePerformIO keyGen

-- FIXME: this is required because of Wallet.Web.Api `type Cors...`
-- I don't really what should be sample for Cors ?
instance ToSample Text where
    toSamples Proxy = fail "ToSample CORS: Not Implemented!"

instance ToSample () where
    toSamples Proxy = singleSample ()

instance ToSample CTx where
    toSamples Proxy = fail "ToSample CTx: Not Implemented!"

instance ToSample CTxMeta where
    toSamples Proxy = fail "ToSample CTxMeta: Not Implemented!"
--
--instance ToSample Tx where
--    toSamples Proxy = singleSample $ Tx [TxIn hsh idx] [out]
--      where ((hsh, idx), (out, _)) = M.toList (genesisUtxo def) !! 0
