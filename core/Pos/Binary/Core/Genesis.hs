-- | Binary instances for Genesis data (such as `StakeDistribution`)

module Pos.Binary.Core.Genesis () where

import           Universum

import           Pos.Binary.Class        (Bi (..), Peek, PokeWithSize,
                                          UnsignedVarInt (..), convertToSizeNPut,
                                          getWord8, label, putField, putS, putWord8S)
import           Pos.Binary.Core.Address ()
import           Pos.Binary.Core.Types   ()
import           Pos.Core.Address        ()
import           Pos.Core.Genesis.Types  (GenesisCoreData (..), StakeDistribution (..))

getUVI :: Peek Word
getUVI = getUnsignedVarInt <$> get

putUVI :: Word -> PokeWithSize ()
putUVI = putS . UnsignedVarInt

instance Bi StakeDistribution where
    get = label "StakeDistribution" $ getWord8 >>= \case
        0 -> FlatStakes <$> getUVI <*> get
        1 -> BitcoinStakes <$> getUVI <*> get
        2 -> RichPoorStakes <$> getUVI <*> get <*> getUVI <*> get
        3 -> pure ExponentialStakes
        4 -> ExplicitStakes <$> get
        5 -> CombinedStakes <$> get <*> get
        _ -> fail "Pos.Binary.Genesis: StakeDistribution: invalid tag"
    sizeNPut = convertToSizeNPut f
      where
        f :: StakeDistribution -> PokeWithSize ()
        f (FlatStakes n total)       = putWord8S 0 <> putUVI n <> putS total
        f (BitcoinStakes n total)    = putWord8S 1 <> putUVI n <> putS total
        f (RichPoorStakes m rs n ps) =
            putWord8S 2 <> putUVI m <> putS rs <>
            putUVI n <> putS ps
        f ExponentialStakes          = putWord8S 3
        f (ExplicitStakes balances)  = putWord8S 4 <> putS balances
        f (CombinedStakes st1 st2)   = putWord8S 5 <> putS st1 <> putS st2

instance Bi GenesisCoreData where
    get = label "GenesisCoreData" $ GenesisCoreData <$> get <*> get <*> get
    sizeNPut =
        putField gcdAddresses
     <> putField gcdDistribution
     <> putField gcdBootstrapBalances
