-- | Binary instances for Genesis data (such as `StakeDistribution`)

module Pos.Binary.Core.Genesis () where

import           Universum

import           Pos.Binary.Class        (Bi (..), Get, Put, UnsignedVarInt (..),
                                          getWord8, label, putWord8)
import           Pos.Binary.Core.Address ()
import           Pos.Binary.Core.Types   ()
import           Pos.Core.Address        ()
import           Pos.Core.Genesis.Types  (GenesisCoreData (..), StakeDistribution (..))

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
        _ -> fail "Pos.Binary.Core.Genesis: StakeDistribution: invalid tag"
    put (FlatStakes n total)       = putWord8 0 >> putUVI n >> put total
    put (BitcoinStakes n total)    = putWord8 1 >> putUVI n >> put total
    put (RichPoorStakes m rs n ps) = putWord8 2 >> putUVI m >> put rs >>
                                     putUVI n >> put ps
    put ExponentialStakes          = putWord8 3
    put (ExplicitStakes balances)  = putWord8 4 >> put balances
    put (CombinedStakes st1 st2)   = putWord8 5 >> put st1 >> put st2

instance Bi GenesisCoreData where
    get = label "GenesisCoreData" $ GenesisCoreData <$> get <*> get <*> get
    put GenesisCoreData {..} = do
        put gcdAddresses
        put gcdDistribution
        put gcdBootstrapBalances

