{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module MarshallingSpec (spec) where

import           Universum

import           Control.Lens (from, to)
import           Data.Aeson
import qualified Data.ByteString as BS
import           Data.SafeCopy hiding (Migrate, Version)
import           Data.Serialize (runGet, runPut)
import           Data.Time (UTCTime (..), fromGregorian)
import           Data.Time.Clock.POSIX (POSIXTime)
import           Data.Typeable (typeRep)
import           Data.Version (Version)
import           Pos.Client.Txp.Util (InputSelectionPolicy)
import           Pos.Core.NetworkMagic (NetworkMagic (..))
import qualified Pos.Crypto as Crypto
import           Servant.API (FromHttpApiData (..), ToHttpApiData (..))
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import qualified Test.QuickCheck.Property as Property

import qualified Cardano.Crypto.Wallet as CCW
import qualified Pos.Chain.Block as Core
import qualified Pos.Chain.Delegation as Core
import qualified Pos.Chain.Ssc as Ssc
import qualified Pos.Chain.Txp as Txp
import qualified Pos.Chain.Update as Core
import qualified Pos.Core as Core
import qualified Pos.Core.Attributes as Core
import qualified Pos.Crypto as Core

import           Test.Pos.Chain.Block.Arbitrary ()
import           Test.Pos.Core.Arbitrary ()

import           Cardano.Wallet.API.Indices
import           Cardano.Wallet.API.Request.Pagination (Page, PerPage)
import           Cardano.Wallet.API.Response (JSONValidationError,
                     UnsupportedMimeTypeError)
import           Cardano.Wallet.API.V1.Types
import           Cardano.Wallet.Kernel.DB.HdWallet (HdRoot)
import           Cardano.Wallet.Kernel.DB.InDb (InDb (..))
import qualified Cardano.Wallet.Kernel.Util.Strict as Strict
import qualified Cardano.Wallet.Util as Util

-- | Tests whether or not some instances (JSON, Bi, etc) roundtrips.
spec :: HasCallStack => Spec
spec = parallel $ describe "Marshalling & Unmarshalling" $ do
    parallel $ describe "Roundtrips" $ do
        aesonRoundtripProp @Account Proxy
        aesonRoundtripProp @AssuranceLevel Proxy
        aesonRoundtripProp @BackupPhrase Proxy
        aesonRoundtripProp @Redemption Proxy
        aesonRoundtripProp @(V1 Core.SoftwareVersion) Proxy
        aesonRoundtripProp @NodeSettings Proxy
        aesonRoundtripProp @Payment Proxy
        aesonRoundtripProp @PaymentDistribution Proxy
        aesonRoundtripProp @NewWallet Proxy
        aesonRoundtripProp @NewAddress Proxy
        aesonRoundtripProp @(V1 Core.Coin) Proxy
        aesonRoundtripProp @(V1 Crypto.PassPhrase) Proxy
        aesonRoundtripProp @(V1 InputSelectionPolicy) Proxy
        aesonRoundtripProp @TimeInfo Proxy
        aesonRoundtripProp @Transaction Proxy
        aesonRoundtripProp @(V1 Core.Timestamp) Proxy
        aesonRoundtripProp @TransactionDirection Proxy
        aesonRoundtripProp @TransactionType Proxy
        aesonRoundtripProp @TransactionStatus Proxy
        aesonRoundtripProp @WalletError Proxy
        aesonRoundtripProp @JSONValidationError Proxy
        aesonRoundtripProp @UnsupportedMimeTypeError Proxy
        aesonRoundtripProp @WalletId Proxy
        aesonRoundtripProp @Wallet Proxy
        aesonRoundtripProp @SlotDuration Proxy
        aesonRoundtripProp @LocalTimeDifference Proxy
        aesonRoundtripProp @BlockchainHeight Proxy
        aesonRoundtripProp @SyncPercentage Proxy
        aesonRoundtripProp @NodeInfo Proxy
        aesonRoundtripProp @SyncState Proxy
        aesonRoundtripProp @EstimatedCompletionTime Proxy
        aesonRoundtripProp @SyncProgress Proxy
        aesonRoundtripProp @SyncThroughput Proxy
        aesonRoundtripProp @AccountIndex Proxy
        aesonRoundtripProp @(V1 AddressOwnership) Proxy
        aesonRoundtripProp @(V1 Version) Proxy

        -- HttpApiData roundtrips
        httpApiDataRoundtripProp @AccountIndex Proxy
        httpApiDataRoundtripProp @(V1 Txp.TxId) Proxy
        httpApiDataRoundtripProp @WalletId Proxy
        httpApiDataRoundtripProp @(V1 Core.Timestamp) Proxy
        httpApiDataRoundtripProp @(V1 Core.Address) Proxy
        httpApiDataRoundtripProp @PerPage Proxy
        httpApiDataRoundtripProp @Page Proxy
        httpApiDataRoundtripProp @Core.Coin Proxy

        -- SafeCopy roundtrips
        safeCopyRoundTrip @(Strict.Maybe Int)
        safeCopyRoundTrip @(InDb Core.Address)
        safeCopyRoundTrip @(InDb Core.AddrAttributes)
        safeCopyRoundTrip @(InDb Core.AddrStakeDistribution)
        safeCopyRoundTrip @(InDb Core.CoinPortion)
        safeCopyRoundTrip @(InDb Core.HDAddressPayload)
        safeCopyRoundTrip @(InDb Core.Coin)
        safeCopyRoundTrip @(InDb Core.Timestamp)
        safeCopyRoundTrip @(InDb Txp.TxAux)
        safeCopyRoundTrip @(InDb Txp.Tx)
        safeCopyRoundTrip @(InDb Txp.TxOut)
        safeCopyRoundTrip @(InDb Txp.TxOutAux)
        safeCopyRoundTrip @(InDb Txp.TxWitness)
        safeCopyRoundTrip @(InDb Txp.TxInWitness)
        safeCopyRoundTrip @(InDb Core.AddrType)
        safeCopyRoundTrip @(InDb (Core.Signature Int))
        safeCopyRoundTrip @(InDb Core.PublicKey)
        safeCopyRoundTrip @(InDb CCW.ChainCode)
        safeCopyRoundTrip @(InDb Txp.TxSigData)
        safeCopyRoundTrip @(InDb Core.RedeemPublicKey)
        safeCopyRoundTrip @(InDb Core.Script)
        safeCopyRoundTrip @(InDb Core.EpochIndex)
        safeCopyRoundTrip @(InDb Core.UnparsedFields)
        safeCopyRoundTrip @(InDb ())
        safeCopyRoundTrip @(InDb Txp.TxIn)
        safeCopyRoundTrip @(InDb Core.MainProof)
        safeCopyRoundTrip @(InDb Ssc.SscProof)
        safeCopyRoundTrip @(InDb Txp.TxProof)
        safeCopyRoundTrip @(InDb (Core.MerkleRoot Txp.Tx))
        safeCopyRoundTrip @(InDb Core.MainExtraHeaderData)
        safeCopyRoundTrip @(InDb Core.BlockVersion)
        safeCopyRoundTrip @(InDb Core.SoftwareVersion)
        safeCopyRoundTrip @(InDb Core.ApplicationName)
        -- safeCopyRoundTrip @(InDb (Core.ProxySignature w)
        -- safeCopyRoundTrip @(InDb (Core.ProxySecretKey w))
        -- safeCopyRoundTrip @(InDb (Core.ProxyCert w))
        safeCopyRoundTrip @(InDb CCW.XSignature)
        safeCopyRoundTrip @(InDb (Core.HeavyDlgIndex))
        safeCopyRoundTrip @(InDb (Core.LightDlgIndices))
        safeCopyRoundTrip @(InDb Core.ChainDifficulty)
        safeCopyRoundTrip @(InDb Core.BlockCount)
        safeCopyRoundTrip @(InDb Core.GenesisBlockHeader)
        safeCopyRoundTrip @(InDb Core.ProtocolMagic)
        safeCopyRoundTrip @(InDb Core.ProtocolMagicId)
        safeCopyRoundTrip @(InDb Core.RequiresNetworkMagic)
        safeCopyRoundTrip @(InDb NetworkMagic)
        safeCopyRoundTrip @(InDb Core.GenesisProof)
        safeCopyRoundTrip @(InDb Core.GenesisConsensusData)
        safeCopyRoundTrip @(InDb Core.GenesisExtraHeaderData)
        safeCopyRoundTrip @(InDb (Core.AddressHash Core.Address'))
        safeCopyRoundTrip @(InDb (Core.Attributes Core.AddrAttributes))
        safeCopyRoundTrip @(InDb (Core.AddrType))
        safeCopyRoundTrip @(InDb Core.SlotId)
        safeCopyRoundTrip @(InDb Core.LocalSlotIndex)
        safeCopyRoundTrip @(InDb Core.BlockHeader)
        safeCopyRoundTrip @(InDb Core.MainBlockHeader)
        safeCopyRoundTrip @(InDb Core.MainConsensusData)
        safeCopyRoundTrip @(InDb Core.BlockSignature)
        safeCopyRoundTrip @(IxSet HdRoot)

        -- Other roundtrips
        generalRoundtripProp "UTC time" Util.showApiUtcTime Util.parseApiUtcTime

    describe "Invariants" $ do
        describe "password" $ do
            it "empty string decodes to empty password" $
                jsonString "" `decodesTo` (== V1 (Crypto.emptyPassphrase))
            it "base-16 string of length 32 decodes to nonempty password" $
                jsonString (fromString $ replicate 64 'a')
                    `decodesTo` (/= V1 (Crypto.emptyPassphrase))
            it "invalid length password decoding fails" $
                -- currently passphrase should be either empty or of length 32
                decodingFails @(V1 Crypto.PassPhrase) "aabbcc" Proxy

    describe "Timestamp Parsing" $ do
        describe "ToIndex" $ do
            let toIndex' :: Text -> Maybe (V1 Core.Timestamp)
                toIndex' = toIndex (Proxy @Transaction)
            it "can parse an ISO8601 UTC formatted date" $ do
                toIndex' "1999-10-12"
                    `shouldBe`
                        Just (UTCTime (fromGregorian 1999 10 12) 0
                            ^. from Core.timestampToUTCTimeL . to V1
                            )
            it "can parse an ISO8601 UTC formatted datetime (seconds)" $ do
                toIndex' "1999-10-12T22:15:31.123"
                    `shouldBe`
                        Just (
                            UTCTime
                                (fromGregorian 1999 10 12)
                                ((22 * 60 * 60) + (15 * 60) + 31.123)
                            ^. from Core.timestampToUTCTimeL . to V1
                            )
            it "can parse an ISO8601 UTC formatted datetime (fractional)" $ do
                toIndex' "1999-10-12T22:15:37"
                    `shouldBe`
                        Just (
                            UTCTime
                                (fromGregorian 1999 10 12)
                                ((22 * 60 * 60) + (15 * 60) + 37)
                            ^. from Core.timestampToUTCTimeL . to V1
                            )
            it "can parse an integral timestamp" $ do
                toIndex' "123456789"
                    `shouldBe`
                        Just ((123456789 :: POSIXTime)
                            ^. from Core.timestampSeconds . to V1
                            )
            it "can parse an fractional timestamp" $ do
                toIndex' "123456789.123"
                    `shouldBe`
                        Just ((123456789.123 :: POSIXTime)
                            ^. from Core.timestampSeconds . to V1
                            )

aesonRoundtrip :: (Arbitrary a, ToJSON a, FromJSON a, Eq a, Show a) => proxy a -> Property
aesonRoundtrip (_ :: proxy a) = forAll arbitrary $ \(s :: a) -> do
    eitherDecode (encode s) === Right s

aesonRoundtripProp
    :: (Arbitrary a, ToJSON a, FromJSON a, Eq a, Show a, Typeable a)
    => proxy a -> Spec
aesonRoundtripProp proxy =
    prop ("Aeson " <> show (typeRep proxy) <> " roundtrips") (aesonRoundtrip proxy)

httpApiDataRoundtrip :: (Arbitrary a, FromHttpApiData a, ToHttpApiData a, Eq a, Show a) => proxy a -> Property
httpApiDataRoundtrip (_ :: proxy a) = forAll arbitrary $ \(s :: a) -> do
    parseQueryParam (toQueryParam s) === Right s

httpApiDataRoundtripProp
    :: (Arbitrary a, ToHttpApiData a, FromHttpApiData a, Eq a, Show a, Typeable a)
    => proxy a -> Spec
httpApiDataRoundtripProp proxy =
    prop ("HttpApiData " <> show (typeRep proxy) <> " roundtrips") (httpApiDataRoundtrip proxy)

safeCopyRoundTrip
    :: forall a
    . (HasCallStack, Arbitrary a, SafeCopy a, Show a, Eq a, Typeable a)
    => Spec
safeCopyRoundTrip = prop propName $ \(a :: a) ->
    runGet safeGet (runPut (safePut a)) === Right a
  where
    propName = "Safe Copy Roundtrip for: " <> show (typeRep (Proxy @a))

generalRoundtrip
    :: (Arbitrary from, Eq from, Show from, Show e)
    => (from -> to) -> (to -> Either e from) -> Property
generalRoundtrip generalEncode generalDecode = property $ \a ->
    case generalDecode (generalEncode a) of
        Right a' -> a === a'
        Left e   -> property Property.failed{ Property.reason = show e }

generalRoundtripProp
    :: (Arbitrary from, Eq from, Show from, Show e)
    => String -> (from -> to) -> (to -> Either e from) -> Spec
generalRoundtripProp desc generalEncode generalDecode =
    prop (desc <> " roundtrip") $ generalRoundtrip generalEncode generalDecode

decodesTo :: (FromJSON a, Show a) => LByteString -> (a -> Bool) -> Expectation
decodesTo s p = either expectationFailure (`shouldSatisfy` p) $ eitherDecode s

decodingFails :: (FromJSON a, Show a) => LByteString -> proxy a -> Expectation
decodingFails s (_ :: proxy a) = eitherDecode @a s `shouldSatisfy` isLeft

-- | Take a string value, and make a JSON-string from it.
jsonString :: LByteString -> LByteString
jsonString bs = "\"" <> bs <> "\""

instance Arbitrary CCW.XSignature where
    arbitrary = do
        bs <- BS.pack <$> replicateM 64 arbitrary
        case CCW.xsignature bs of
            Left _  -> arbitrary
            Right a -> pure a

instance Arbitrary CCW.ChainCode where
    arbitrary = CCW.ChainCode <$> arbitrary
