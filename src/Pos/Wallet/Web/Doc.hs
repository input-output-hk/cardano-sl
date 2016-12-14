{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Documentation of wallet web API.

module Pos.Wallet.Web.Doc
       ( walletDocsText
       ) where

import           Control.Lens              ((.~), (<>~))
import           Data.Default              (def)
import qualified Data.HashMap.Strict       as HM
import           Data.List                 ((!!))
import qualified Data.Map                  as M
import           Data.Proxy                (Proxy (..))
import           Network.HTTP.Types.Method (methodPost)
import           Servant.API               (Capture)
import           Servant.Docs              (API, DocCapture (..), DocIntro (..),
                                            DocNote (..), ExtraInfo (..),
                                            ToCapture (toCapture), ToSample (toSamples),
                                            defAction, defEndpoint, defaultDocOptions,
                                            docsWith, markdown, method, notes, path,
                                            singleSample)
import qualified Servant.Docs              as SD
import           Universum

import           Pos.Genesis               (genesisAddresses, genesisUtxo)
import           Pos.Types                 (Address, Coin, Tx (..), TxIn (..))
import           Pos.Wallet.Web.Api        (walletApi)

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

instance ToCapture (Capture "from" Word) where
    toCapture Proxy =
        DocCapture
        { _capSymbol = "from"
        , _capDesc = "Index of address from which coins should be sent."
        }

instance ToCapture (Capture "to" Address) where
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

instance ToCapture (Capture "address" Address) where
    toCapture Proxy =
        DocCapture
        { _capSymbol = "address"
        , _capDesc = "Address, history of which should be fetched"
        }

instance ToSample Coin where
    toSamples Proxy = singleSample 100500

instance ToSample Address where
    toSamples Proxy = singleSample $ genesisAddresses !! 0

instance ToSample () where
    toSamples Proxy = singleSample ()

instance ToSample Tx where
    toSamples Proxy = singleSample $ Tx [TxIn hsh idx] [out]
      where ((hsh, idx), out) = M.toList (genesisUtxo def) !! 0
