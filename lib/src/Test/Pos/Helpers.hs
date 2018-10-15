{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}

-- Need this to avoid a warning on the `typeName` helper function.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.Pos.Helpers
       ( canonicalJsonTest
       , canonicalJsonTestWithGen
       ) where

import           Universum

import           Data.Functor.Identity (Identity (..))
import           Data.Typeable (typeRep)
import           Test.Hspec (Spec)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Gen, Property, forAll, (.&&.), (===))
import qualified Text.JSON.Canonical as CanonicalJSON

import           Pos.Util.Json.Canonical (SchemaError)

import           Test.Pos.Binary.Helpers (IdTestingRequiredClassesAlmost,
                     identityTest)

----------------------------------------------------------------------------
-- From/to tests
----------------------------------------------------------------------------

type ToAndFromCanonicalJson a
     = ( CanonicalJSON.ToJSON Identity a
       , CanonicalJSON.FromJSON (Either SchemaError) a
       )

canonicalJsonTest ::
       forall a. (IdTestingRequiredClassesAlmost a, ToAndFromCanonicalJson a)
    => Spec
canonicalJsonTest =
    identityTest @a $ \x ->
        canonicalJsonRenderAndDecode x .&&. canonicalJsonPrettyAndDecode x

-- | Basically the same as `canonicalJsonTest` but tests a given `Gen a`.
canonicalJsonTestWithGen
    :: forall a. (IdTestingRequiredClassesAlmost a, ToAndFromCanonicalJson a)
    => Gen a
    -> Spec
canonicalJsonTestWithGen genA =
    prop (typeName @a) $ forAll genA $ \x ->
        canonicalJsonRenderAndDecode x .&&. canonicalJsonPrettyAndDecode x
  where
    -- GHC 8.2.2 says the `Typeable x` constraint is not necessary, but won't compile
    -- this without it.
    typeName :: forall x. Typeable x => String
    typeName = show $ typeRep (Proxy @a)

canonicalJsonRenderAndDecode
    :: forall a. (IdTestingRequiredClassesAlmost a, ToAndFromCanonicalJson a)
    => a
    -> Property
canonicalJsonRenderAndDecode x =
    let encodedX =
            CanonicalJSON.renderCanonicalJSON $
            runIdentity $ CanonicalJSON.toJSON x
    in canonicalJsonDecodeAndCompare x encodedX

canonicalJsonPrettyAndDecode
    :: forall a. (IdTestingRequiredClassesAlmost a, ToAndFromCanonicalJson a)
    => a
    -> Property
canonicalJsonPrettyAndDecode x =
    let encodedX =
            encodeUtf8 $
            CanonicalJSON.prettyCanonicalJSON $
            runIdentity $ CanonicalJSON.toJSON x
    in canonicalJsonDecodeAndCompare x encodedX

canonicalJsonDecodeAndCompare
    :: forall a. (IdTestingRequiredClassesAlmost a, ToAndFromCanonicalJson a)
    => a
    -> LByteString
    -> Property
canonicalJsonDecodeAndCompare x encodedX =
    let decodedValue =
            either (error . toText) identity $
            CanonicalJSON.parseCanonicalJSON encodedX
        decodedX =
            either (error . pretty @SchemaError) identity $
            CanonicalJSON.fromJSON decodedValue
    in decodedX === x
