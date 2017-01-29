-- | Binary instances for Genesis data (such as `StakeDistribution`)

module Pos.Binary.Genesis () where

import           Data.Binary.Get                (Get, getWord8)
import           Data.Binary.Put                (Put, putWord8)
import           Universum

import           Pos.Binary.Class               (Bi (..), UnsignedVarInt (..))
import           Pos.Binary.Ssc.GodTossing.Core ()
import           Pos.Genesis.Types              (GenesisData (..), StakeDistribution (..))

getUVI :: Get Word
getUVI = getUnsignedVarInt <$> get

putUVI :: Word -> Put
putUVI = put . UnsignedVarInt

instance Bi StakeDistribution where
    get = getWord8 >>= \case
        0 -> FlatStakes <$> getUVI <*> get
        1 -> BitcoinStakes <$> getUVI <*> get
        2 -> TestnetStakes <$> get <*> getUVI <*> getUVI
        3 -> pure ExponentialStakes
        4 -> ExplicitStakes <$> get
        _ -> fail "Pos.Binary.Genesis: StakeDistribution: invalid tag"
    put (FlatStakes n total)      = putWord8 0 >> putUVI n >> put total
    put (BitcoinStakes n total)   = putWord8 1 >> putUVI n >> put total
    put (TestnetStakes total m n) = putWord8 2 >> put total >>
                                    putUVI m >> putUVI n
    put ExponentialStakes         = putWord8 3
    put (ExplicitStakes balances) = putWord8 4 >> put balances

instance Bi GenesisData where
    get = GenesisData <$> get <*> get <*> get
    put GenesisData {..} = put gdAddresses >>
                           put gdDistribution >>
                           put gdVssCertificates
