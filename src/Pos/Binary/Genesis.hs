-- | Binary instances for Genesis data (such as `StakeDistribution`)

module Pos.Binary.Genesis () where

import           Data.Binary.Get   (Get, getWord8, label)
import           Data.Binary.Put   (Put, putWord8)
import           Universum

import           Pos.Binary.Class  (Bi (..), UnsignedVarInt (..))
import           Pos.Genesis.Types (GenesisData (..), StakeDistribution (..))

getUVI :: Get Word
getUVI = getUnsignedVarInt <$> get

putUVI :: Word -> Put
putUVI = put . UnsignedVarInt

instance Bi StakeDistribution where
    get = label "StakeDistribution" $ getWord8 >>= \case
        0 -> FlatStakes <$> getUVI <*> get
        1 -> BitcoinStakes <$> getUVI <*> get
        2 -> RichPoorStakes <$> getUVI <*> get <*> getUVI <*> get
        3 -> pure ExponentialStakes
        4 -> ExplicitStakes <$> get
        5 -> CombinedStakes <$> get <*> get
        _ -> fail "Pos.Binary.Genesis: StakeDistribution: invalid tag"
    put (FlatStakes n total)       = putWord8 0 >> putUVI n >> put total
    put (BitcoinStakes n total)    = putWord8 1 >> putUVI n >> put total
    put (RichPoorStakes m rs n ps) = putWord8 2 >> putUVI m >> put rs >>
                                     putUVI n >> put ps
    put ExponentialStakes          = putWord8 3
    put (ExplicitStakes balances)  = putWord8 4 >> put balances
    put (CombinedStakes st1 st2)   = putWord8 5 >> put st1 >> put st2

instance Bi GenesisData where
    get = label "GenesisData" $ GenesisData <$> get <*> get <*> get
    put GenesisData {..} = put gdAddresses >>
                           put gdDistribution >>
                           put gdBootstrapBalances

