-- | This module tests Binary instances.

module Test.Pos.Types.Identity.PassPhraseSpec
       ( spec
       ) where

import           Pos.Crypto.SafeSigning   (mkPassPhrase, passPhraseToByteString)
import           Test.Hspec               (Spec, describe, it)
import           Test.QuickCheck          (Property, property, (===))
import           Test.QuickCheck.Property (ioProperty)
import           Universum

import           Test.Pos.Util            ()

spec :: Spec
spec = describe "PassPhrase" $ do
    describe "Identity testing" $ do
        it "To ByteString convertion" $
            property conversionTest

conversionTest :: ByteString -> Property
conversionTest bs = ioProperty $
    mkPassPhrase bs >>= passPhraseToByteString >>= return . (=== bs)
