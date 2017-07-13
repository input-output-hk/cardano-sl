-- | Binary instances for Genesis data (such as `StakeDistribution`)

module Pos.Binary.Core.Genesis () where

import           Universum

import           Pos.Binary.Class        (Bi (..), Peek, PokeWithSize,
                                          UnsignedVarInt (..), convertToSizeNPut,
                                          getWord8, label, labelS, putField, putS,
                                          putWord8S)
import qualified Pos.Binary.Cbor         as Cbor
import           Pos.Binary.Core.Address ()
import           Pos.Binary.Core.Types   ()
import           Pos.Core.Address        ()
import           Pos.Core.Genesis.Types  (GenesisCoreData (..), StakeDistribution (..),
                                          mkGenesisCoreData)

getUVI :: Peek Word
getUVI = getUnsignedVarInt <$> get

putUVI :: Word -> PokeWithSize ()
putUVI = putS . UnsignedVarInt

instance Bi StakeDistribution where
    get = label "StakeDistribution" $ getWord8 >>= \case
        0 -> FlatStakes <$> getUVI <*> get
        1 -> BitcoinStakes <$> getUVI <*> get
        2 -> RichPoorStakes <$> getUVI <*> get <*> getUVI <*> get
        3 -> ExponentialStakes <$> getUVI
        4 -> CustomStakes <$> get
        _ -> fail "Pos.Binary.Genesis: StakeDistribution: invalid tag"
    sizeNPut = labelS "StakeDistribution" $ convertToSizeNPut f
      where
        f :: StakeDistribution -> PokeWithSize ()
        f (FlatStakes n total)       = putWord8S 0 <> putUVI n <> putS total
        f (BitcoinStakes n total)    = putWord8S 1 <> putUVI n <> putS total
        f (RichPoorStakes m rs n ps) =
            putWord8S 2 <> putUVI m <> putS rs <>
            putUVI n <> putS ps
        f (ExponentialStakes n)      = putWord8S 3 <> putUVI n
        f (CustomStakes coins)       = putWord8S 4 <> putS coins

instance Cbor.Bi StakeDistribution where
  encode input = case input of
    FlatStakes w c             -> Cbor.encodeListLen 3 <> Cbor.encode (0 :: Word8) <> Cbor.encode w <> Cbor.encode c
    BitcoinStakes w c          -> Cbor.encodeListLen 3 <> Cbor.encode (1 :: Word8) <> Cbor.encode w <> Cbor.encode c
    RichPoorStakes w1 c1 w2 c2 -> Cbor.encodeListLen 5 <> Cbor.encode (2 :: Word8)
                                                       <> Cbor.encode w1
                                                       <> Cbor.encode c1
                                                       <> Cbor.encode w2
                                                       <> Cbor.encode c2
    ExponentialStakes w        -> Cbor.encodeListLen 2 <> Cbor.encode (3 :: Word8) <> Cbor.encode w
    CustomStakes c             -> Cbor.encodeListLen 2 <> Cbor.encode (4 :: Word8) <> Cbor.encode c
  decode = do
    _ <- Cbor.decodeListLen
    tag <- Cbor.decode @Word8
    case tag of
      0 -> FlatStakes        <$> Cbor.decode <*> Cbor.decode
      1 -> BitcoinStakes     <$> Cbor.decode <*> Cbor.decode
      2 -> RichPoorStakes    <$> Cbor.decode <*> Cbor.decode <*> Cbor.decode <*> Cbor.decode
      3 -> ExponentialStakes <$> Cbor.decode
      4 -> CustomStakes      <$> Cbor.decode
      _ -> fail "Pos.Binary.Genesis: StakeDistribution: invalid tag"


instance Bi GenesisCoreData where
    get = label "GenesisCoreData" $ do
        addrDistribution <- get
        bootstrapStakeholders <- get
        case mkGenesisCoreData addrDistribution bootstrapStakeholders of
            Left e  -> fail $ "Couldn't construct genesis data: " <> e
            Right x -> pure x
    sizeNPut =
        labelS "GenesisCoreData" $
            putField gcdAddrDistribution <>
            putField gcdBootstrapStakeholders

instance Cbor.Bi GenesisCoreData where
  encode (UnsafeGenesisCoreData addr stakes) = Cbor.encodeListLen 2 <> Cbor.encode addr <> Cbor.encode stakes
  decode = do
    Cbor.enforceSize "GenesisCoreData" 2
    addrDistribution      <- Cbor.decode
    bootstrapStakeholders <- Cbor.decode
    case mkGenesisCoreData addrDistribution bootstrapStakeholders of
        Left e  -> fail $ "Couldn't construct genesis data: " <> e
        Right x -> pure x
