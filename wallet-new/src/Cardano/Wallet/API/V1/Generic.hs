{-# LANGUAGE PolyKinds #-}

module Cardano.Wallet.API.V1.Generic
       ( gtoJsend
       , gparseJsend
       , gconsNames
       ) where

import           Universum hiding (All, Generic)

import           Data.Aeson
import           Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import           Generics.SOP
import           Generics.SOP.JSON (JsonInfo (..), JsonOptions (..), Tag (..), defaultJsonOptions,
                                    jsonInfo)

import           Cardano.Wallet.Util (mkJsonKey)
import           Pos.Util.Util (aesonError)

--
-- Helper proxies
--

pt :: Proxy ToJSON
pt = Proxy

allpt :: Proxy (All ToJSON)
allpt = Proxy

pf :: Proxy FromJSON
pf = Proxy

allpf :: Proxy (All FromJSON)
allpf = Proxy

--
-- JSON encoding/decoding
--

-- | Returns `JsonInfo` for type (from `json-sop` package)
-- for representing a type in a JSend format.
jsendInfo
    :: forall a. (HasDatatypeInfo a, SListI (Code a))
    => Proxy a -> NP JsonInfo (Code a)
jsendInfo pa = jsonInfo pa $ defaultJsonOptions
    { jsonFieldName = const mkJsonKey
    }

-- | Generic method which makes JSON `Value` from a Haskell value in
-- JSend format.
gtoJsend
    :: forall a. (Generic a, HasDatatypeInfo a, All2 ToJSON (Code a))
    => a -> Value
gtoJsend a = hcollapse $
    hcliftA2 allpt gtoJsend'
    (jsendInfo (Proxy :: Proxy a))
    (unSOP $ from a)

gtoJsend'
    :: All ToJSON xs
    => JsonInfo xs -> NP I xs -> K Value xs
gtoJsend' (JsonZero n) Nil =
    jsendValue (Tag n) (Object mempty)
gtoJsend' (JsonOne tag) (I a :* Nil) =
    jsendValue tag (toJSON a)
gtoJsend' (JsonMultiple tag) cs =
    jsendValue tag . Array . V.fromList . hcollapse $
    hcliftA pt (K . toJSON . unI) cs
gtoJsend' (JsonRecord tag fields) cs =
    jsendValue tag . Object . HM.fromList . hcollapse $
    hcliftA2 pt (\(K field) (I a) -> K (toText field, toJSON a)) fields cs

-- | Generic method which parses a Haskell value from given `Value`.
gparseJsend
    :: forall a. (Generic a, HasDatatypeInfo a, All2 FromJSON (Code a))
    => Value -> Parser a
gparseJsend v = to <$> gparseJsend' v (jsendInfo (Proxy :: Proxy a))

gparseJsend'
    :: forall (xss :: [[*]]). All2 FromJSON xss
    => Value -> NP JsonInfo xss -> Parser (SOP I xss)
gparseJsend' v infos = asum . hcollapse $
    hcliftA2 allpf (parseJsendConstructor v) infos
    (injections :: NP (Injection (NP I) xss) xss)

parseJsendConstructor
    :: forall (xss :: [[*]]) (xs :: [*]). All FromJSON xs
    => Value -> JsonInfo xs -> Injection (NP I) xss xs -> K (Parser (SOP I xss)) xs
parseJsendConstructor v info (Fn inj) = K $ do
    vals <- parseJsendValues info v
    return $ SOP $ unK (inj vals)

parseJsendValues
    :: forall (xs :: [*]). All FromJSON xs
    => JsonInfo xs -> Value -> Parser (NP I xs)
parseJsendValues (JsonZero n) =
    unJsendValue (Tag n) $
    const $ return Nil
parseJsendValues (JsonOne tag) =
    unJsendValue tag $ \o -> do
        v <- parseJSON o
        return $ I v :* Nil
parseJsendValues (JsonMultiple tag) =
    unJsendValue tag $
    withArray "Array" $ \arr ->
        case fromList (V.toList arr) of
            Nothing   -> aesonError "Too few values!"
            Just vals ->
                let mkVal :: FromJSON a => K Value a -> Parser a
                    mkVal = parseJSON . unK
                in hsequence $ hcliftA pf mkVal vals
parseJsendValues (JsonRecord tag fields) =
    unJsendValue tag $
    withObject "Object" $ \o ->
        let getField :: FromJSON a => K String a -> Parser a
            getField (K name) = o .: toText name
        in hsequence $ hcliftA pf getField fields

-- | Helper function which makes a JSON value in JSend format
-- from a constructor tag and object with constructor's arguments
jsendValue :: Tag -> Value -> K Value a
jsendValue NoTag v   = K v
jsendValue (Tag t) v = K $ Object $
    HM.fromList [ ("message", String $ toText t)
                , ("diagnostic", v)
                ]

-- | Helper function to parse value in JSend format if desired constructor
-- is known.
unJsendValue :: Tag -> (Value -> Parser a) -> Value -> Parser a
unJsendValue NoTag f = f
unJsendValue (Tag n) f = withObject ("Expected JSend object with message `" <> n <> "`") $
    \o -> do
        msg <- o .: "message"
        guard $ n == msg
        val <- o .: "diagnostic"
        f val

--
-- Misc
--

gconsName' :: forall xs. ConstructorInfo xs -> K ConstructorName xs
gconsName' (Infix n _ _)   = K n
gconsName' (Constructor n) = K n
gconsName' (Record n _)    = K n

gconsInfos
    :: forall a. (HasDatatypeInfo a, SListI (Code a))
    => Proxy a -> NP ConstructorInfo (Code a)
gconsInfos pa = case datatypeInfo pa of
    Newtype _ _ conInfo -> conInfo :* Nil
    ADT _ _ consInfo    -> consInfo

gconsNames
    :: forall a. (HasDatatypeInfo a, SListI (Code a))
    => Proxy a -> [Text]
gconsNames = map toText . hcollapse . hliftA gconsName' . gconsInfos
