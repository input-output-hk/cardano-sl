{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | API documentation.

module Pos.Web.Doc
       ( baseDocsText
       , gtDocsText
       ) where

import           Control.Lens        ((<>~))
import           Data.Default        (Default (def))
import qualified Data.HashMap.Strict as HM
import           Data.Proxy          (Proxy (..))
import           Servant.API         (Capture, QueryParam)
import           Servant.Docs        (API, DocCapture (..), DocIntro (..), DocNote (..),
                                      DocQueryParam (..), ExtraInfo (..),
                                      ParamKind (Normal), ToCapture (toCapture),
                                      ToParam (toParam), ToSample (toSamples), defAction,
                                      defEndpoint, defaultDocOptions, docsWith, markdown,
                                      notes, path, singleSample)
import qualified Servant.Docs        as SD
import           Universum

import           Pos.Crypto          (Hash, PublicKey, deterministicKeyGen, unsafeHash)
import           Pos.Genesis         (genesisLeaders, genesisUtxo)
import           Pos.Types           (EpochIndex, SharedSeed (..), SlotId (..),
                                      SlotLeaders)
import           Pos.Web.Api         (baseNodeApi, gtNodeApi)
import           Pos.Web.Types       (GodTossingStage (..))

baseDocs :: API
baseDocs = docsWith defaultDocOptions intros extras (SD.pretty baseNodeApi)

baseDocsText :: Text
baseDocsText = toText $ markdown baseDocs

gtDocs :: API
gtDocs = docsWith defaultDocOptions intros extras (SD.pretty gtNodeApi)

gtDocsText :: Text
gtDocsText = toText $ markdown gtDocs

intros :: [DocIntro]
intros =
    [ DocIntro
          "Documentation of cardano-node web API"
          ["This is very first version, don't expect it to be smart."]
    ]

-- [CSL-234]: this is unsafe solution, but I didn't manage to make
-- safe one work :(
extras :: ExtraInfo api
extras =
    ExtraInfo . HM.fromList $
    [ (defEndpoint & path <>~ ["head_hash"], defAction & notes <>~ headHashNotes)
    , (defEndpoint & path <>~ ["local_txs_num"], defAction & notes <>~ localTxsNumNotes)
    ]
  where
    headHashNotes =
        [ DocNote
          { _noteTitle = "Description"
          , _noteBody =
              [ "Returns hash of header of block which is head of our blockchain."
              ]
          }
        ]
    localTxsNumNotes =
        [ DocNote
          { _noteTitle = "Description"
          , _noteBody =
              [ "Returns number of transactions stored locally."
              ]
          }
        ]

----------------------------------------------------------------------------
-- Orphan instances
----------------------------------------------------------------------------

instance ToParam (QueryParam "epoch" EpochIndex) where
    toParam Proxy =
        DocQueryParam
        { _paramName = "epoch"
        , _paramValues = ["0", "1", "2", "…"]
        , _paramDesc =
            "Epoch for which leaders are requested. By default current epoch is used."
        , _paramKind = Normal
        }

instance ToCapture (Capture "enable" Bool) where
    toCapture Proxy =
        DocCapture
        { _capSymbol = "enable"
        , _capDesc = "whether participation should be enabled or disabled"
        }

instance ToSample SlotId where
    toSamples Proxy =
        [ ( "0-th slot of 0-th epoch (very beginning)"
          , SlotId
            { siSlot = 0
            , siEpoch = 0
            })
        , ( "2-nd slot of 3-rd epoch (very beginning)"
          , SlotId
            { siSlot = 2
            , siEpoch = 3
            })
        ]

instance ToSample SlotLeaders where
    toSamples Proxy = singleSample $ genesisLeaders $ genesisUtxo def

instance ToSample PublicKey where
    toSamples Proxy =
        singleSample $
        fst $
        fromMaybe (panic "Pos.Web.Doc: deterministicKeyGen failed") $
        deterministicKeyGen "I have to sleep on it, because. "

instance ToSample (Hash skovoroda) where
    toSamples Proxy = singleSample $ unsafeHash @Text "skovoroda"

instance ToSample Word where
    toSamples Proxy = singleSample 322

instance ToSample SharedSeed where
    toSamples Proxy =
        singleSample $
        SharedSeed
            "\208\191\208\176\209\130\208\176\208\186\208\191\208\176\209\130\208\176\208\186\208\191\208\176\209\130\208\176\208\186\209\210"

instance ToSample () where
    toSamples Proxy = singleSample ()

instance ToSample GodTossingStage where
    toSamples Proxy = [ ("Commitment stage", CommitmentStage)
                      , ("Opening stage", OpeningStage)
                      , ("Shares stage", SharesStage)
                      , ("Ordinary stage", OrdinaryStage)
                      ]
