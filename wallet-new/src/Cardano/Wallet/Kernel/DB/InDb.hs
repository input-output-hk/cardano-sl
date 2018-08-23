{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

module Cardano.Wallet.Kernel.DB.InDb
    ( InDb(..)
    , fromDb
    ) where

import           Universum

import           Control.Arrow ((***))
import           Control.Lens.TH (makeLenses)
import           Crypto.Hash (Digest, digestFromByteString)
import qualified Crypto.Sign.Ed25519 as Ed25519
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as Map
import qualified Data.SafeCopy as SC
import           Data.Serialize (Put)
import qualified Data.Set as Set
import qualified Data.Time.Units
import qualified Data.Vector as V
import           Test.QuickCheck (Arbitrary (..))

import qualified Pos.Chain.Block as Core
import qualified Pos.Core as Core
import qualified Pos.Core.Attributes as Core
import qualified Pos.Core.Delegation as Core
import qualified Pos.Core.Ssc as Ssc
import qualified Pos.Core.Txp as Txp
import qualified Pos.Core.Update as Core
import qualified Pos.Crypto as Core

import qualified Cardano.Crypto.Wallet as CCW

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

{-------------------------------------------------------------------------------
  Wrap core types so that we can make independent serialization decisions
-------------------------------------------------------------------------------}

-- | Wrapped type (with potentially different 'SafeCopy' instance)
--
-- NOTE:
--
-- 1. We want to be independent from the 'SafeCopy' instances in core. For this
--    reason, we wrap all core types in 'InDb', and provide explicit 'SafeCopy'
--    instances for 'InDb SomeCoreType'.
-- 2. We never use 'InDb' for types that we have control over in the wallet
--    itself.
-- 3. To avoid too much code bloat everywhere else, we don't nest 'InDb';
--    i.e., we don't use @InDb (.... InDb ....)@. Instead, we use 'InDb' only
--    /around/ (possibly nested) core types; for example, we use
--    @InDb (Map SomeCoreType SomeOtherCoreType)@. We then translate this to
--    @Map (InDb SomeCoreType) (InDb SomeOtherCoreType)@ in the 'SafeCopy'
--    instances themselves, so that in the rest of the code we don't have to
--    do too much wrapping and unwrapping.
--
-- A consequence of these rules is that something like
--
-- > safePut (InDb x) = safePut x
--
-- is correct /only/ if @x@ has a primitive type (i.e., not one defined in
-- the Cardano core, but in the Haskell base libraries).
newtype InDb a = InDb { _fromDb :: a }
    deriving (Eq, Show, Ord, Buildable)

instance Functor InDb where
    fmap f = InDb . f . _fromDb

instance Applicative InDb where
    pure = InDb
    InDb f <*> InDb x = InDb (f x)

instance (Arbitrary a) => Arbitrary (InDb a) where
    arbitrary = InDb <$> arbitrary

makeLenses ''InDb

--------------------------------------------------------------------------------
-- SafeCopy instances for InDb types
--
-- Notice that when serializing and deserializing, a type like `InDb foo` will
-- distribute `InDb` among the non-primitive types that make up `foo`. This
-- choice doesn't show up in the types.

instance SC.SafeCopy (InDb Core.Address) where
    getCopy = SC.contain $ do
        InDb (r :: Core.AddressHash Core.Address') <- SC.safeGet
        InDb (a :: Core.Attributes Core.AddrAttributes) <- SC.safeGet
        InDb (t :: Core.AddrType) <- SC.safeGet
        pure (InDb (Core.Address r a t))
    putCopy (InDb (Core.Address r a t)) = SC.contain $ do
        SC.safePut (InDb (r :: Core.AddressHash Core.Address'))
        SC.safePut (InDb (a :: Core.Attributes Core.AddrAttributes))
        SC.safePut (InDb (t :: Core.AddrType))

instance SC.SafeCopy (InDb Core.AddrAttributes) where
    getCopy = SC.contain $ do
        yiap :: Maybe (InDb Core.HDAddressPayload) <- SC.safeGet
        InDb (ast :: Core.AddrStakeDistribution) <- SC.safeGet
        pure (InDb (Core.AddrAttributes (fmap _fromDb yiap) ast))
    putCopy (InDb (Core.AddrAttributes yap asr)) = SC.contain $ do
        SC.safePut (fmap InDb yap)
        SC.safePut (InDb asr)

instance SC.SafeCopy (InDb Core.AddrStakeDistribution) where
    getCopy = SC.contain $ fmap InDb $ do
        SC.safeGet >>= \case
          0 -> pure Core.BootstrapEraDistr
          1 -> Core.SingleKeyDistr <$> fmap _fromDb SC.safeGet
          2 -> Core.UnsafeMultiKeyDistr <$> fmap _fromDb SC.safeGet
          (n :: Word8) -> fail
            $ "Expected one of 0,1,2 for tag of AddrStakeDistribution, got: "
            <> show n
    putCopy (InDb x) = SC.contain $ case x of
        Core.BootstrapEraDistr -> SC.safePut (0 :: Word8)
        Core.SingleKeyDistr a -> do
            SC.safePut (1 :: Word8)
            SC.safePut (InDb (a :: Core.StakeholderId))
        Core.UnsafeMultiKeyDistr m -> do
            SC.safePut (2 :: Word8)
            SC.safePut (InDb (m :: Map Core.StakeholderId Core.CoinPortion))

instance SC.SafeCopy (InDb Core.CoinPortion) where
    getCopy = SC.contain $ do
        w :: Word64 <- SC.safeGet
        pure (InDb (Core.CoinPortion w))
    putCopy (InDb (Core.CoinPortion w)) = SC.contain $ do
        SC.safePut (w :: Word64)

instance SC.SafeCopy (InDb Core.HDAddressPayload) where
    getCopy = SC.contain $ do
        bs :: B.ByteString <- SC.safeGet
        pure (InDb (Core.HDAddressPayload bs))
    putCopy (InDb (Core.HDAddressPayload bs)) = SC.contain $ do
        SC.safePut (bs :: B.ByteString)

instance SC.SafeCopy (InDb Core.Coin) where
    getCopy = SC.contain $ do
       w :: Word64 <- SC.safeGet
       pure (InDb (Core.Coin w))
    putCopy (InDb (Core.Coin w)) = SC.contain $ do
       SC.safePut (w :: Word64)

instance SC.SafeCopy (InDb Core.SlotId) where
    getCopy = SC.contain $ do
      InDb (e :: Core.EpochIndex) <- SC.safeGet
      InDb (s :: Core.LocalSlotIndex) <- SC.safeGet
      pure (InDb (Core.SlotId e s))
    putCopy (InDb (Core.SlotId e s)) = SC.contain $ do
      SC.safePut (InDb e)
      SC.safePut (InDb s)

instance SC.SafeCopy (InDb Core.Timestamp) where
    getCopy = SC.contain $ do
        msi :: Integer <- SC.safeGet
        let ms :: Data.Time.Units.Microsecond = fromInteger msi
        pure (InDb (Core.Timestamp ms))
    putCopy (InDb (Core.Timestamp ms)) = SC.contain $ do
        let msi :: Integer = toInteger (ms :: Data.Time.Units.Microsecond)
        SC.safePut msi

instance SC.SafeCopy (InDb Txp.TxAux) where
    getCopy = SC.contain $ do
        InDb (tx :: Txp.Tx) <- SC.safeGet
        InDb (txw :: Txp.TxWitness) <- SC.safeGet
        pure (InDb (Txp.TxAux tx txw))
    putCopy (InDb (Txp.TxAux tx txw)) = SC.contain $ do
        SC.safePut (InDb tx)
        SC.safePut (InDb txw)

instance SC.SafeCopy (InDb Txp.Tx) where
    getCopy = SC.contain $ do
        InDb (i :: NonEmpty Txp.TxIn) <- SC.safeGet
        InDb (o :: NonEmpty Txp.TxOut) <- SC.safeGet
        InDb (a :: Txp.TxAttributes) <- SC.safeGet
        pure (InDb (Txp.UnsafeTx i o a))
    putCopy (InDb (Txp.UnsafeTx i o a)) = SC.contain $ do
        SC.safePut (InDb i)
        SC.safePut (InDb o)
        SC.safePut (InDb a)

instance SC.SafeCopy (InDb Txp.TxOut) where
    getCopy = SC.contain $ do
        InDb (a :: Core.Address) <- SC.safeGet
        InDb (c :: Core.Coin) <- SC.safeGet
        pure (InDb (Txp.TxOut a c))
    putCopy (InDb (Txp.TxOut a c)) = SC.contain $ do
        SC.safePut (InDb a)
        SC.safePut (InDb c)

instance SC.SafeCopy (InDb Txp.TxOutAux) where
    getCopy = SC.contain $ do
        InDb (x :: Txp.TxOut) <- SC.safeGet
        pure (InDb (Txp.TxOutAux x))
    putCopy (InDb (Txp.TxOutAux x)) = SC.contain $ do
        SC.safePut (InDb x)

instance SC.SafeCopy (InDb Txp.TxWitness) where
    getCopy = SC.contain $ do
        xsi :: [InDb Txp.TxInWitness] <- SC.safeGet
        let v :: V.Vector Txp.TxInWitness = V.fromList (map _fromDb xsi)
        pure (InDb v)
    putCopy (InDb v) = SC.contain $ do
        let xsi :: [InDb Txp.TxInWitness] = map InDb (V.toList v)
        SC.safePut xsi

instance SC.SafeCopy (InDb Txp.TxInWitness) where
    getCopy = SC.contain $ fmap InDb $ do
        SC.safeGet >>= \case
            0 -> Txp.PkWitness
                <$> fmap _fromDb SC.safeGet
                <*> fmap _fromDb SC.safeGet
            1 -> Txp.ScriptWitness
                <$> fmap _fromDb SC.safeGet
                <*> fmap _fromDb SC.safeGet
            2 -> Txp.RedeemWitness
                <$> fmap _fromDb SC.safeGet
                <*> fmap _fromDb SC.safeGet
            3 -> Txp.UnknownWitnessType
                <$> SC.safeGet
                <*> SC.safeGet
            (n :: Word8) -> fail
                $ "Expected 0,1,2,3 for tag of TxInWitness, got: "
                <> show n

    putCopy (InDb x) = SC.contain $ case x of
        Txp.PkWitness a b -> do
            SC.safePut (0 :: Word8)
            SC.safePut (InDb (a :: Core.PublicKey))
            SC.safePut (InDb (b :: Txp.TxSig))
        Txp.ScriptWitness a b -> do
            SC.safePut (1 :: Word8)
            SC.safePut (InDb (a :: Core.Script))
            SC.safePut (InDb (b :: Core.Script))
        Txp.RedeemWitness a b -> do
            SC.safePut (2 :: Word8)
            SC.safePut (InDb (a :: Core.RedeemPublicKey))
            SC.safePut (InDb (b :: Core.RedeemSignature Txp.TxSigData))
        Txp.UnknownWitnessType a b -> do
            SC.safePut (3 :: Word8)
            SC.safePut (a :: Word8)
            SC.safePut (b :: B.ByteString)

instance SC.SafeCopy (InDb Core.AddrType) where
    getCopy = SC.contain $ fmap InDb $ do
        SC.safeGet >>= \case
            0 -> pure Core.ATPubKey
            1 -> pure Core.ATScript
            2 -> pure Core.ATRedeem
            3 -> Core.ATUnknown <$> SC.safeGet
            (n :: Word8) -> fail
                $ "Expected 0,1,2,3 for tag of AddrType, got: "
                <> show n

    putCopy (InDb x) = SC.contain $ case x of
        Core.ATPubKey -> SC.safePut (0 :: Word8)
        Core.ATScript -> SC.safePut (1 :: Word8)
        Core.ATRedeem -> SC.safePut (2 :: Word8)
        Core.ATUnknown a -> do
            SC.safePut (3 :: Word8)
            SC.safePut (a :: Word8)

instance SC.SafeCopy (InDb (Core.Signature a)) where
    getCopy = SC.contain $ do
        bs :: B.ByteString <- SC.safeGet
        Right xsig <- pure (CCW.xsignature bs)
        pure (InDb (Core.Signature xsig))

    putCopy (InDb (Core.Signature xsig)) = SC.contain $ do
        let bs = CCW.unXSignature xsig
        SC.safePut bs

instance SC.SafeCopy (InDb Core.PublicKey) where
    getCopy = SC.contain $ do
        pkbs :: B.ByteString <- SC.safeGet
        InDb (cc :: CCW.ChainCode) <- SC.safeGet
        let xpub = CCW.XPub pkbs cc
        pure (InDb (Core.PublicKey xpub))
    putCopy (InDb (Core.PublicKey xpub)) = SC.contain $ do
        let CCW.XPub pkbs cc = xpub
        SC.safePut pkbs
        SC.safePut (InDb cc)

instance SC.SafeCopy (InDb CCW.ChainCode) where
    getCopy = SC.contain $ do
        bs :: B.ByteString <- SC.safeGet
        pure (InDb (CCW.ChainCode bs))
    putCopy (InDb (CCW.ChainCode bs)) = SC.contain $ do
        SC.safePut bs

instance SC.SafeCopy (InDb Txp.TxSigData) where
    getCopy = SC.contain $ do
        InDb (h :: Core.Hash Txp.Tx) <- SC.safeGet
        pure (InDb (Txp.TxSigData h))
    putCopy (InDb (Txp.TxSigData h)) = SC.contain $ do
        SC.safePut (InDb h)

instance SC.SafeCopy (InDb Core.RedeemPublicKey) where
    getCopy = SC.contain $ do
        bs :: B.ByteString <- SC.safeGet
        pure (InDb (Core.RedeemPublicKey (Ed25519.PublicKey bs)))
    putCopy (InDb (Core.RedeemPublicKey pk)) = SC.contain $ do
        SC.safePut (Ed25519.openPublicKey pk :: B.ByteString)

instance SC.SafeCopy (InDb (Core.RedeemSignature a)) where
    getCopy = SC.contain $ do
        bs :: B.ByteString <- SC.safeGet
        pure (InDb (Core.RedeemSignature (Ed25519.Signature bs)))
    putCopy (InDb (Core.RedeemSignature pk)) = SC.contain $ do
        SC.safePut (Ed25519.unSignature pk :: B.ByteString)

instance SC.SafeCopy (InDb h) => SC.SafeCopy (InDb (Core.Attributes h)) where
    getCopy = SC.contain $ do
        InDb (d :: h) <- SC.safeGet
        InDb (r :: Core.UnparsedFields) <- SC.safeGet
        pure (InDb (Core.Attributes d r))
    putCopy (InDb (Core.Attributes d r)) = SC.contain $ do
        SC.safePut (InDb d)
        SC.safePut (InDb r)

instance SC.SafeCopy (InDb Core.Script) where
    getCopy = SC.contain $ do
        v :: Word16 <- SC.safeGet
        bs :: B.ByteString <- SC.safeGet
        pure (InDb (Core.Script v bs))
    putCopy (InDb (Core.Script v bs)) = SC.contain $ do
        SC.safePut (v :: Word16)
        SC.safePut (bs :: B.ByteString)

instance SC.SafeCopy (InDb Core.LocalSlotIndex) where
    getCopy = SC.contain $ do
        w :: Word16 <- SC.safeGet
        pure (InDb (Core.UnsafeLocalSlotIndex w))
    putCopy (InDb (Core.UnsafeLocalSlotIndex w)) = SC.contain $ do
        SC.safePut (w :: Word16)

instance SC.SafeCopy (InDb Core.EpochIndex) where
    getCopy = SC.contain $ do
        w :: Word64 <- SC.safeGet
        pure (InDb (Core.EpochIndex w))
    putCopy (InDb (Core.EpochIndex w)) = SC.contain $ do
        SC.safePut (w :: Word64)

instance SC.SafeCopy (InDb Core.UnparsedFields) where
    getCopy = SC.contain $ do
        m :: Map Word8 BL.ByteString <- SC.safeGet
        pure (InDb (Core.UnparsedFields m))
    putCopy (InDb (Core.UnparsedFields m)) = SC.contain $ do
        SC.safePut m

instance SC.SafeCopy (InDb ()) where
    getCopy = SC.contain (fmap InDb SC.safeGet)
    putCopy (InDb ()) = SC.contain (SC.safePut ())

instance (SC.SafeCopy (InDb a), SC.SafeCopy (InDb b))
    => SC.SafeCopy (InDb (a, b)) where
    getCopy = SC.contain $ do
        InDb (a :: a) <- SC.safeGet
        InDb (b :: b) <- SC.safeGet
        pure (InDb (a, b))
    putCopy (InDb (a, b)) = SC.contain $ do
        SC.safePut (InDb a)
        SC.safePut (InDb b)

instance SC.SafeCopy (InDb Txp.TxIn) where
    getCopy = SC.contain $
        SC.safeGet >>= \case
            0 -> do InDb txId <- SC.safeGet
                    w         <- SC.safeGet
                    pure (InDb (Txp.TxInUtxo txId w))
            1 -> do w <- SC.safeGet
                    b <- SC.safeGet
                    pure (InDb (Txp.TxInUnknown w b))
            (n :: Word8) -> fail
                $  "Expected one of 0,1 for TxIn tag, got: "
                <> show n

    putCopy (InDb a) = SC.contain $ case a of
        Txp.TxInUtxo (txId :: Txp.TxId) (w :: Word32) -> do
            SC.safePut (0 :: Word8)
            SC.safePut (InDb txId)
            SC.safePut w
        Txp.TxInUnknown (w :: Word8) (b :: ByteString) -> do
            SC.safePut (1 :: Word8)
            SC.safePut w
            SC.safePut b

instance (SC.SafeCopy (InDb a)) => SC.SafeCopy (InDb (NonEmpty a)) where
    getCopy = SC.contain $ do
        xsi :: [InDb a] <- SC.safeGet
        case NEL.nonEmpty xsi of
            Nothing  -> fail
                $ "Expected at least one element in non-empty list."
            Just nxs -> pure (InDb (fmap _fromDb nxs))
    putCopy (InDb sa) = SC.contain $ do
        let xsi :: [InDb a] = map InDb (NEL.toList sa)
        SC.safePut xsi

instance (SC.SafeCopy (InDb a), Ord a) => SC.SafeCopy (InDb (Set a)) where
    getCopy = SC.contain $ do
        xsi :: [InDb a] <- SC.safeGet
        pure (InDb (Set.fromList (map _fromDb xsi)))
    putCopy (InDb sa) = SC.contain $ do
        let xsi :: [InDb a] = map InDb (Set.toList sa)
        SC.safePut xsi

instance SC.SafeCopy (InDb Core.BlockHeader) where
    getCopy = SC.contain $ do
        constrTag <- SC.safeGet
        case constrTag :: Word8 of
            0 -> do -- BlockHeaderGenesis
                genBlockHeader <- SC.safeGet
                pure (Core.BlockHeaderGenesis <$> genBlockHeader)
            1 -> do -- BlockHeaderMain
                mainBlockHeader <- SC.safeGet
                pure (Core.BlockHeaderMain <$> mainBlockHeader)
            _ ->
                fail
                    $ "Expected a tag of 1 or 0 for BlockHeader, got: "
                    <> show constrTag
    putCopy (InDb blockHeader) = SC.contain $ do
        case blockHeader of
            Core.BlockHeaderGenesis genBlockHeader -> do
                SC.safePut (0 :: Word8)
                SC.safePut (InDb genBlockHeader)
            Core.BlockHeaderMain mainBlockHeader -> do
                SC.safePut (1 :: Word8)
                SC.safePut (InDb mainBlockHeader)

-- The instances for 'MainBlockHeader' and 'GenesisBlockHeader' seem like
-- duplicates, but the constructor fields use type families on the 'Blockchain'
-- class for the actual fields, so they are actually totally different types.
instance SC.SafeCopy (InDb Core.MainBlockHeader) where
    getCopy = SC.contain $ do
        InDb protocolMagic <- SC.safeGet
        InDb prevBlock <- SC.safeGet
        InDb bodyProof <- SC.safeGet
        InDb consensus <- SC.safeGet
        InDb extra <- SC.safeGet
        pure . InDb $
            Core.UnsafeGenericBlockHeader
                protocolMagic
                prevBlock
                bodyProof
                consensus
                extra
    putCopy (InDb (Core.UnsafeGenericBlockHeader a b c d e)) = SC.contain $ do
        safePutDb a
        safePutDb b
        safePutDb c
        safePutDb d
        safePutDb e

safePutDb :: SC.SafeCopy (InDb a) => a -> Put
safePutDb = SC.safePut . InDb

instance SC.SafeCopy (InDb Core.MainProof) where
    getCopy = SC.contain $ do
        InDb txProof <- SC.safeGet
        InDb mpcProof <- SC.safeGet
        InDb proxySKsProof <- SC.safeGet
        InDb updateProof <- SC.safeGet
        pure . InDb $
            Core.MainProof
                txProof
                mpcProof
                proxySKsProof
                updateProof
    putCopy (InDb (Core.MainProof txP mpcP proxySKsP updateP)) = SC.contain $ do
        SC.safePut (InDb txP)
        SC.safePut (InDb mpcP)
        SC.safePut (InDb proxySKsP)
        SC.safePut (InDb updateP)

instance SC.SafeCopy (InDb Ssc.SscProof) where
    getCopy = SC.contain $ do
        constrTag <- SC.safeGet
        case constrTag :: Word8 of
            0 -> do -- CommitmentsProof
                InDb hash <- SC.safeGet
                InDb cert <- SC.safeGet
                pure . InDb $
                    Ssc.CommitmentsProof hash cert
            1 -> do -- OpeningsProof
                InDb hash <- SC.safeGet
                InDb cert <- SC.safeGet
                pure . InDb $
                    Ssc.OpeningsProof hash cert
            2 -> do -- SharesProof
                InDb hash <- SC.safeGet
                InDb cert <- SC.safeGet
                pure . InDb $
                    Ssc.SharesProof hash cert
            3 -> do -- CertificatesProof
                InDb cert <- SC.safeGet
                pure . InDb $
                    Ssc.CertificatesProof cert
            _ ->
                fail
                    $ "Expected 0,1,2,3 tag when parsing SscProof, got: "
                    <> show constrTag

    putCopy (InDb proof) = SC.contain $ do
        case proof of
            Ssc.CommitmentsProof hash cert -> do
                SC.safePut (0 :: Word8)
                safePutDb hash
                safePutDb cert
            Ssc.OpeningsProof hash cert -> do
                SC.safePut (1 :: Word8)
                safePutDb hash
                safePutDb cert
            Ssc.SharesProof hash cert -> do
                SC.safePut (2 :: Word8)
                safePutDb hash
                safePutDb cert
            Ssc.CertificatesProof cert -> do
                SC.safePut (3 :: Word8)
                safePutDb cert

instance SC.SafeCopy (InDb Txp.TxProof) where
    getCopy = SC.contain $ do
        number <- SC.safeGet
        InDb txRoot <- SC.safeGet
        InDb witness <- SC.safeGet
        pure . InDb $
            Txp.TxProof
                number
                txRoot
                witness
    putCopy (InDb (Txp.TxProof n root witness)) = SC.contain $ do
        SC.safePut n
        safePutDb root
        safePutDb witness

instance SC.SafeCopy (InDb (Core.MerkleRoot Txp.Tx)) where
    getCopy = SC.contain $ do
        InDb rootHash <- SC.safeGet
        pure . InDb $
            Core.MerkleRoot rootHash
    putCopy (InDb (Core.MerkleRoot rootHash)) = SC.contain $ do
        safePutDb rootHash

instance SC.SafeCopy (InDb Core.MainConsensusData) where
    getCopy = SC.contain $ do
        slotId <- SC.safeGet
        leaderKey <- SC.safeGet
        difficulty <- SC.safeGet
        sig <- SC.safeGet
        pure $ Core.MainConsensusData
            <$> slotId
            <*> leaderKey
            <*> difficulty
            <*> sig

    putCopy (InDb (Core.MainConsensusData slot l d sig)) = SC.contain $ do
        safePutDb slot
        safePutDb l
        safePutDb d
        safePutDb sig

instance SC.SafeCopy (InDb Core.MainExtraHeaderData) where
    getCopy = SC.contain $ do
        blockVers <- SC.safeGet
        softVers <- SC.safeGet
        attrs <- SC.safeGet
        ebDataProof <- SC.safeGet
        pure $ Core.MainExtraHeaderData
            <$> blockVers
            <*> softVers
            <*> attrs
            <*> ebDataProof

    putCopy (InDb (Core.MainExtraHeaderData b s a e)) = SC.contain $ do
        safePutDb b
        safePutDb s
        safePutDb a
        safePutDb e

instance SC.SafeCopy (InDb Core.BlockVersion) where
    getCopy = SC.contain $ do
        (\a b c -> InDb (Core.BlockVersion a b c))
            <$> SC.safeGet
            <*> SC.safeGet
            <*> SC.safeGet

    putCopy (InDb (Core.BlockVersion a b c)) = SC.contain $ do
        SC.safePut a
        SC.safePut b
        SC.safePut c

instance SC.SafeCopy (InDb Core.SoftwareVersion) where
    getCopy = SC.contain $ do
        appName <- SC.safeGet
        appNumber <- SC.safeGet
        pure $ Core.SoftwareVersion
            <$> appName
            <*> pure appNumber

    putCopy (InDb (Core.SoftwareVersion name number)) = SC.contain $ do
        safePutDb name
        SC.safePut number

instance SC.SafeCopy (InDb Core.ApplicationName) where
    getCopy = SC.contain $ do
        InDb . Core.ApplicationName <$> SC.safeGet

    putCopy (InDb (Core.ApplicationName name)) = SC.contain $ do
        SC.safePut name

instance SC.SafeCopy (InDb Core.BlockSignature) where
    getCopy = SC.contain $ do
        constrTag <- SC.safeGet
        case constrTag :: Word8 of
            0 -> do -- BlockSignature
                sigMain <- SC.safeGet
                pure $ Core.BlockSignature
                    <$> sigMain
            1 -> do -- BlockPSignatureLight
                sigLight <- SC.safeGet
                pure $ Core.BlockPSignatureLight
                    <$> sigLight
            2 -> do -- BlockPSignatureHeavy
                sigHeavy <- SC.safeGet
                pure $ Core.BlockPSignatureHeavy
                    <$> sigHeavy
            _ -> fail
                $ "Expected one of 0,1,2 while reading BlockSignature, got: "
                <> show constrTag

    putCopy (InDb blockSig) = SC.contain $ do
        case blockSig of
            Core.BlockSignature o -> do
                SC.safePut (0 :: Word8)
                safePutDb o
            Core.BlockPSignatureLight o -> do
                SC.safePut (1 :: Word8)
                safePutDb o
            Core.BlockPSignatureHeavy o -> do
                SC.safePut (2 :: Word8)
                safePutDb o

instance SC.SafeCopy (InDb w)
    => SC.SafeCopy (InDb (Core.ProxySignature w a)) where
    getCopy = SC.contain $ do
        psk <- SC.safeGet
        sig <- SC.safeGet
        pure $ Core.ProxySignature
            <$> psk
            <*> sig

    putCopy (InDb (Core.ProxySignature psk sig)) = SC.contain $ do
        safePutDb psk
        safePutDb sig

instance SC.SafeCopy (InDb w) => SC.SafeCopy (InDb (Core.ProxySecretKey w)) where
    getCopy = SC.contain $ do
        omega <- SC.safeGet
        issuer <- SC.safeGet
        delegate <- SC.safeGet
        cert <- SC.safeGet
        pure $ Core.UnsafeProxySecretKey
            <$> omega
            <*> issuer
            <*> delegate
            <*> cert

    putCopy (InDb (Core.UnsafeProxySecretKey o i d c)) = SC.contain $ do
        safePutDb o
        safePutDb i
        safePutDb d
        safePutDb c

instance SC.SafeCopy (InDb (Core.ProxyCert w)) where
    getCopy = SC.contain $ do
        xsig <- SC.safeGet
        pure $ Core.ProxyCert <$> xsig

    putCopy (InDb (Core.ProxyCert xsig)) = SC.contain $ do
        safePutDb xsig

instance SC.SafeCopy (InDb CCW.XSignature) where
    getCopy = SC.contain $ do
        bs <- SC.safeGet
        case CCW.xsignature bs of
            Left err ->
                fail err
            Right xsig ->
                pure . InDb $ xsig

    putCopy (InDb xsig) = SC.contain $ do
        SC.safePut (CCW.unXSignature xsig)

instance SC.SafeCopy (InDb (Core.HeavyDlgIndex)) where
    getCopy = SC.contain $ do
        epoch <- SC.safeGet
        pure $ Core.HeavyDlgIndex
            <$> epoch

    putCopy (InDb (Core.HeavyDlgIndex epoch)) = SC.contain $ do
        safePutDb epoch

instance SC.SafeCopy (InDb (Core.LightDlgIndices)) where
    getCopy = SC.contain $ do
        idx0 <- SC.safeGet
        idx1 <- SC.safeGet
        pure $ curry Core.LightDlgIndices
            <$> idx0
            <*> idx1

    putCopy (InDb (Core.LightDlgIndices (i0, i1))) = SC.contain $ do
        safePutDb i0
        safePutDb i1

instance SC.SafeCopy (InDb Core.ChainDifficulty) where
    getCopy = SC.contain $ do
        blockCount <- SC.safeGet
        pure $ Core.ChainDifficulty
            <$> blockCount

    putCopy (InDb (Core.ChainDifficulty i)) = SC.contain $ do
        safePutDb i

instance SC.SafeCopy (InDb Core.BlockCount) where
    getCopy = SC.contain $ do
        InDb . Core.BlockCount <$> SC.safeGet

    putCopy (InDb (Core.BlockCount i)) = SC.contain $ do
        SC.safePut i

instance SC.SafeCopy (InDb Core.GenesisBlockHeader) where
    getCopy = SC.contain $ do
        InDb protocolMagic <- SC.safeGet
        InDb prevBlock <- SC.safeGet
        InDb bodyProof <- SC.safeGet
        InDb consensus <- SC.safeGet
        InDb extra <- SC.safeGet
        pure . InDb $
            Core.UnsafeGenericBlockHeader
                protocolMagic
                prevBlock
                bodyProof
                consensus
                extra

    putCopy (InDb (Core.UnsafeGenericBlockHeader pm pb bp co ex)) = SC.contain $ do
        safePutDb pm
        safePutDb pb
        safePutDb bp
        safePutDb co
        safePutDb ex

instance SC.SafeCopy (InDb Core.ProtocolMagic) where
    getCopy = SC.contain $ do
        InDb . Core.ProtocolMagic <$> SC.safeGet

    putCopy (InDb (Core.ProtocolMagic i)) = SC.contain $ do
        SC.safePut i

instance SC.SafeCopy (InDb Core.GenesisProof) where
    getCopy = SC.contain $ do
        hashLeaders <- SC.safeGet
        pure $ Core.GenesisProof
            <$> hashLeaders

    putCopy (InDb (Core.GenesisProof hash)) = SC.contain $ do
        safePutDb hash

instance SC.SafeCopy (InDb Core.GenesisConsensusData) where
    getCopy = SC.contain $ do
        epoch <- SC.safeGet
        difficulty <- SC.safeGet
        pure $ Core.GenesisConsensusData
            <$> epoch
            <*> difficulty

    putCopy (InDb (Core.GenesisConsensusData e d)) = SC.contain $ do
        safePutDb e
        safePutDb d

instance SC.SafeCopy (InDb Core.GenesisExtraHeaderData) where
    getCopy = SC.contain $ do
        attrs <- SC.safeGet
        pure $ Core.GenesisExtraHeaderData
            <$> attrs

    putCopy (InDb (Core.GenesisExtraHeaderData attrs)) = SC.contain $ do
        safePutDb attrs

instance (SC.SafeCopy (InDb a), SC.SafeCopy (InDb b), Ord a)
    => SC.SafeCopy (InDb (Map a b)) where
    getCopy = SC.contain $ do
        xsi :: [(InDb a, InDb b)] <- SC.safeGet
        let m :: Map a b = Map.fromList (map (_fromDb *** _fromDb) xsi)
        pure (InDb m)
    putCopy (InDb m) = SC.contain $ do
      let xsi :: [(InDb a, InDb b)] = map (InDb *** InDb) (Map.toList m)
      SC.safePut xsi

instance forall algo a.
    (Core.HashAlgorithm algo)
    => SC.SafeCopy (InDb (Core.AbstractHash algo a)) where
    getCopy = SC.contain $ do
        b :: B.ByteString <- SC.safeGet
        Just (d :: Digest algo) <- pure (digestFromByteString b)
        pure (InDb (Core.AbstractHash d))
    putCopy (InDb (Core.AbstractHash (d :: Digest algo))) = SC.contain $ do
        SC.safePut (BA.convert d :: B.ByteString)
