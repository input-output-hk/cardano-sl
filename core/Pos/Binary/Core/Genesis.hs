-- | Binary instances for Genesis data (such as `StakeDistribution`)

module Pos.Binary.Core.Genesis () where

import           Universum

import           Pos.Binary.Class        (Bi (..), Peek, PokeWithSize,
                                          UnsignedVarInt (..), convertToSizeNPut,
                                          getWord8, label, labelS, putField, putS,
                                          putWord8S)
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
