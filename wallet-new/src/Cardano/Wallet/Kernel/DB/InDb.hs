{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.Kernel.DB.InDb (
    InDb(..)
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
import qualified Data.Set as Set
import qualified Data.Time.Units
import qualified Data.Vector as V

import qualified Pos.Chain.Block as Core
import qualified Pos.Chain.Txp as Core
import qualified Pos.Core as Core
import qualified Pos.Core.Attributes as Core
import qualified Pos.Core.Txp as Txp
import qualified Pos.Core.Update as Core
import qualified Pos.Crypto as Core

import qualified Cardano.Crypto.Wallet as CCW

{-------------------------------------------------------------------------------
  Wrap core types so that we can make independent serialization decisions
-------------------------------------------------------------------------------}

-- | Wrapped type (with potentially different 'SC' instance)
newtype InDb a = InDb { _fromDb :: a }
  deriving (Eq, Ord)

instance Functor InDb where
  fmap f = InDb . f . _fromDb

instance Applicative InDb where
  pure = InDb
  InDb f <*> InDb x = InDb (f x)

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
    SC.safePut asr

instance SC.SafeCopy (InDb Core.AddrStakeDistribution) where
  getCopy = SC.contain $ fmap InDb $ do
    SC.safeGet >>= \case
      0 -> pure Core.BootstrapEraDistr
      1 -> Core.SingleKeyDistr <$> fmap _fromDb SC.safeGet
      2 -> Core.UnsafeMultiKeyDistr <$> fmap _fromDb SC.safeGet
      (_ :: Word8) -> empty
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
      3 -> Txp.RedeemWitness <$> SC.safeGet <*> SC.safeGet
      (_ :: Word8) -> empty
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
      (_ :: Word8) -> empty
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
    SC.safePut (Ed25519.unPublicKey pk :: B.ByteString)

instance SC.SafeCopy (InDb (Core.RedeemSignature a)) where
  getCopy = SC.contain $ do
    bs :: B.ByteString <- SC.safeGet
    pure (InDb (Core.RedeemSignature (Ed25519.Signature bs)))
  putCopy (InDb (Core.RedeemSignature pk)) = SC.contain $ do
    SC.safePut (Ed25519.unSignature pk :: B.ByteString)

instance forall h. SC.SafeCopy (InDb h)
  => SC.SafeCopy (InDb (Core.Attributes h)) where
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

instance forall a b.
  (SC.SafeCopy (InDb a), SC.SafeCopy (InDb b))
  => SC.SafeCopy (InDb (a, b)) where
  getCopy = SC.contain $ do
    InDb (a :: a) <- SC.safeGet
    InDb (b :: b) <- SC.safeGet
    pure (InDb (a, b))
  putCopy (InDb (a, b)) = SC.contain $ do
    SC.safePut (InDb a)
    SC.safePut (InDb b)

instance SC.SafeCopy (InDb Txp.TxIn) where
  getCopy = SC.contain $ fmap InDb $ do
    SC.safeGet >>= \case
      0 -> Txp.TxInUtxo <$> SC.safeGet <*> SC.safeGet
      1 -> Txp.TxInUnknown <$> SC.safeGet <*> SC.safeGet
      (_ :: Word8) -> empty
  putCopy (InDb a) = SC.contain $ case a of
    Txp.TxInUtxo (txId :: Txp.TxId) (w :: Word32) -> do
      SC.safePut (0 :: Word8)
      SC.safePut (InDb txId)
      SC.safePut w
    Txp.TxInUnknown (w :: Word8) (b :: ByteString) -> do
      SC.safePut (1 :: Word8)
      SC.safePut w
      SC.safePut b

instance forall a.
    (SC.SafeCopy (InDb a))
    => SC.SafeCopy (InDb (NonEmpty a)) where
    getCopy = SC.contain $ do
      xsi :: [InDb a] <- SC.safeGet
      case NEL.nonEmpty xsi of
         Nothing -> empty
         Just nxs -> pure (InDb (fmap _fromDb nxs))
    putCopy (InDb sa) = SC.contain $ do
      let xsi :: [InDb a] = map InDb (NEL.toList sa)
      SC.safePut xsi

instance forall a.
    (SC.SafeCopy (InDb a), Ord a)
    => SC.SafeCopy (InDb (Set a)) where
    getCopy = SC.contain $ do
      xsi :: [InDb a] <- SC.safeGet
      pure (InDb (Set.fromList (map _fromDb xsi)))
    putCopy (InDb sa) = SC.contain $ do
      let xsi :: [InDb a] = map InDb (Set.toList sa)
      SC.safePut xsi

instance forall a b.
    (SC.SafeCopy (InDb a), SC.SafeCopy (InDb b), Ord a)
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
