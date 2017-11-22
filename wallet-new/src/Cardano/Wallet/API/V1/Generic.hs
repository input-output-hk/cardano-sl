{-# LANGUAGE PolyKinds #-}

module Cardano.Wallet.API.V1.Generic
       ( gToJsend
       ) where

import           Universum hiding (All, Generic)

import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Text (pack)
import qualified Data.Vector as V
import           Generics.SOP
import           Generics.SOP.JSON (JsonInfo (..), Tag (..), defaultJsonOptions, jsonInfo)

pt :: Proxy ToJSON
pt = Proxy

allpt :: Proxy (All ToJSON)
allpt = Proxy

-- | mda
gToJsend
    :: forall a. (Generic a, HasDatatypeInfo a, All2 ToJSON (Code a))
    => a -> Value
gToJsend a = hcollapse $
    hcliftA2 allpt gToJsend'
    (jsonInfo (Proxy :: Proxy a) defaultJsonOptions)
    (unSOP $ from a)

gToJsend'
    :: All ToJSON xs
    => JsonInfo xs -> NP I xs -> K Value xs
gToJsend' (JsonZero n) Nil =
    messageDiagnosticValue (Tag n) (Object mempty)
gToJsend' (JsonOne tag) (I a :* Nil) =
    messageDiagnosticValue tag (toJSON a)
gToJsend' (JsonMultiple tag) cs =
    messageDiagnosticValue tag . Array . V.fromList . hcollapse $
    hcliftA pt (K . toJSON . unI) cs
gToJsend' (JsonRecord tag fields) cs =
    messageDiagnosticValue tag . Object . HM.fromList . hcollapse $
    hcliftA2 pt (\(K field) (I a) -> K (pack field, toJSON a)) fields cs

messageDiagnosticValue :: Tag -> Value -> K Value a
messageDiagnosticValue NoTag v   = K v
messageDiagnosticValue (Tag t) v = K $ Object $
    HM.fromList [ ("message", String $ pack t)
                , ("diagnostic", v)
                ]
