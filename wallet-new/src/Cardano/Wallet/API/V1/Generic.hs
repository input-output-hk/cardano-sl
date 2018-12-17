{-# LANGUAGE PolyKinds #-}

module Cardano.Wallet.API.V1.Generic
    ( jsendErrorGenericToJSON
    , jsendErrorGenericParseJSON
    , gconsNames
    , gconsName
    ) where

import           Universum hiding (All, Generic)

import           Data.Aeson (GFromJSON, Object, ToJSON, Value (..), Zero,
                     genericParseJSON, object, tagSingleConstructors,
                     withObject, (.:), (.=))
import           Data.Aeson.Types (Parser)
import           Data.List ((!!))
import           GHC.Generics

import           Cardano.Wallet.API.Response.JSend (HasDiagnostic (..),
                     ResponseStatus (..))

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Lazy as HM
import qualified Generics.SOP as SOP

--
-- Misc
--

-- | Get the ADT constructor's name of the given value
gconsName
  :: forall a. (SOP.Generic a, SOP.HasDatatypeInfo a)
  => a -> Text
gconsName a =
  gconsNames (Proxy @a) !! SOP.hindex (SOP.from a)

-- | Get all constructors names available of an ADT
gconsNames
    :: forall a. (SOP.HasDatatypeInfo a, SOP.SListI (SOP.Code a))
    => Proxy a -> [Text]
gconsNames =
    map toText . SOP.hcollapse . SOP.hliftA (SOP.K . SOP.constructorName) . gconsInfos


--
-- JSendError Encoding helper
--
jsendErrorGenericToJSON ::
    ( GDiagnosticToJSON (Rep a)
    , HasDiagnostic a
    , Generic a
    , SOP.Generic a
    , SOP.HasDatatypeInfo a
    ) => a -> Value
jsendErrorGenericToJSON a = object
    [ "message"     .= gconsName a
    , "status"      .= ErrorStatus
    , "diagnostic"  .= gDiagnosticToJSON (getDiagnosticKey a) (from a)
    ]

jsendErrorGenericParseJSON ::
    ( Generic a
    , GFromJSON Zero (Rep a)
    ) => Value
    -> Parser a
jsendErrorGenericParseJSON = withObject "JSEndError" $ \o -> do
    message    <- o .: "message"
    diagnostic <- o .: "diagnostic" >>= parseDiagnostic
    genericParseJSON opts $  object
        [ "tag"      .= String message
        , "contents" .= diagnostic
        ]
  where
    opts :: Aeson.Options
    opts = Aeson.defaultOptions { tagSingleConstructors = True }

    parseDiagnostic :: Object -> Parser Value
    parseDiagnostic hm =
        case HM.toList hm of
            []           -> pure (object mempty)
            [(_, value)] -> pure value
            _            -> fail "Invalid ToJSON encoding for JSEndError"


--
-- INTERNALS
--

gconsInfos
    :: forall a. (SOP.HasDatatypeInfo a)
    => Proxy a
    -> SOP.NP SOP.ConstructorInfo (SOP.Code a)
gconsInfos pa = case SOP.datatypeInfo pa of
    SOP.Newtype _ _ conInfo -> conInfo SOP.:* SOP.Nil
    SOP.ADT _ _ consInfo    -> consInfo


-- | This class helps us define generically errors JSON instances without
-- relying on partial field in records.
-- This is used to encode the diagnostic object of an error, as a singleton
-- with field 'Text' whenever there's one.
--
-- NOTE: We haven't defined instances for everything because we do not want to
-- suppport all kind of error structures, but only sums with a unary or
-- nullary constructors.
class GDiagnosticToJSON (f :: * -> *) where
    gDiagnosticToJSON :: Text -> f a -> Value

instance (GDiagnosticToJSON f) => GDiagnosticToJSON (M1 i c f) where
    gDiagnosticToJSON k (M1 f) = gDiagnosticToJSON k f

instance (GDiagnosticToJSON f, GDiagnosticToJSON g) => GDiagnosticToJSON (f :+: g) where
    gDiagnosticToJSON k (L1 f) = gDiagnosticToJSON k f
    gDiagnosticToJSON k (R1 g) = gDiagnosticToJSON k g

instance (ToJSON c) => GDiagnosticToJSON (K1 i c) where
    gDiagnosticToJSON k (K1 c) = object [ k .= c ]

instance GDiagnosticToJSON U1 where
    gDiagnosticToJSON _ _ = object mempty
