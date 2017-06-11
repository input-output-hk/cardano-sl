-- | Binary instances for Genesis data (such as `StakeDistribution`)

module Pos.Binary.Genesis () where

import           Universum

import           Pos.Binary.Class  (Bi (..), Peek, PokeWithSize, UnsignedVarInt (..),
                                    convertToSizeNPut, getWord8, label, pokeWithSize,
                                    putField, putWord8WithSize)
import           Pos.Genesis.Types (GenesisData (..), StakeDistribution (..))

getUVI :: Peek Word
getUVI = getUnsignedVarInt <$> get

putUVI :: Word -> PokeWithSize ()
putUVI = pokeWithSize . UnsignedVarInt

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
        f (FlatStakes n total)       = putWord8WithSize 0 <> putUVI n <> pokeWithSize total
        f (BitcoinStakes n total)    = putWord8WithSize 1 <> putUVI n <> pokeWithSize total
        f (RichPoorStakes m rs n ps) =
            putWord8WithSize 2 <> putUVI m <> pokeWithSize rs <>
            putUVI n <> pokeWithSize ps
        f ExponentialStakes          = putWord8WithSize 3
        f (ExplicitStakes balances)  = putWord8WithSize 4 <> pokeWithSize balances
        f (CombinedStakes st1 st2)   = putWord8WithSize 5 <> pokeWithSize st1 <> pokeWithSize st2

instance Bi GenesisData where
    get = label "GenesisData" $ GenesisData <$> get <*> get <*> get
    sizeNPut =
        putField gdAddresses
     <> putField gdDistribution
     <> putField gdBootstrapBalances

