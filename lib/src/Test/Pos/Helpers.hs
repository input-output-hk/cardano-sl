{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Pos.Helpers
       ( canonicalJsonTest
       ) where

import           Universum

import           Data.Functor.Identity (Identity (..))
import           Test.Hspec (Spec)
import           Test.QuickCheck (Property, (.&&.), (===))
import qualified Text.JSON.Canonical as CanonicalJSON

import           Pos.Core.Genesis (SchemaError)

import           Test.Pos.Binary.Helpers (IdTestingRequiredClassesAlmost, identityTest)

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
  where
    canonicalJsonRenderAndDecode x =
        let encodedX =
                CanonicalJSON.renderCanonicalJSON $
                runIdentity $ CanonicalJSON.toJSON x
        in canonicalJsonDecodeAndCompare x encodedX
    canonicalJsonPrettyAndDecode x =
        let encodedX =
                encodeUtf8 $
                CanonicalJSON.prettyCanonicalJSON $
                runIdentity $ CanonicalJSON.toJSON x
        in canonicalJsonDecodeAndCompare x encodedX
    canonicalJsonDecodeAndCompare ::
           a
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
