-- | Core types. TODO: we need to have a meeting, come up with project
-- structure and follow it.

module Pos.Types.Core
       (
         -- * Coin
         Coin
       , CoinPortion
       , coinF
       , getCoinPortion
       , mkCoin
       , unsafeCoinPortion
       , unsafeGetCoin

        -- * Slotting
       , EpochIndex (..)
       , HasEpochIndex (..)
       , FlatSlotId
       , LocalSlotIndex (..)
       , SlotId (..)
       , EpochOrSlot (..)
       , HasEpochOrSlot (..)
       , Timestamp (..)
       , slotIdF
       , epochOrSlot

        -- * Address
       , Address (..)
       , AddressHash
       , StakeholderId

        -- * ChainDifficulty
       , ChainDifficulty (..)
       , HasDifficulty (..)

        -- * Version
       , ApplicationName (..)
       , BlockVersion (..)
       , NumSoftwareVersion
       , SoftwareVersion (..)

       -- * HeaderHash related types and functions
       , BlockHeaderStub
       , HeaderHash
       , HasHeaderHash (..)
       , headerHashF
       ) where

import           Control.Exception   (assert)
import           Control.Lens        (Getter, to)
import           Crypto.Hash         (Blake2s_224)
import           Data.Data           (Data)
import           Data.Hashable       (Hashable)
import           Data.Ix             (Ix)
import           Data.Text.Buildable (Buildable)
import qualified Data.Text.Buildable as Buildable
import           Data.Time.Units     (Microsecond)
import           Formatting          (Format, bprint, build, int, ords, (%))
import           Serokell.AcidState  ()
import           Universum

import           Pos.Crypto          (AbstractHash, Hash, PublicKey)
import           Pos.Script.Type     (Script)

----------------------------------------------------------------------------
-- Coin
----------------------------------------------------------------------------

-- | Coin is the least possible unit of currency.
newtype Coin = Coin
    { getCoin :: Word64
    } deriving (Show, Ord, Eq, Bounded, Generic, Hashable, Data, NFData)

instance Buildable Coin where
    build (Coin n) = bprint (int%" coin(s)") n

-- | Make Coin from Word64.
mkCoin :: Word64 -> Coin
mkCoin = Coin
{-# INLINE mkCoin #-}

-- | Coin formatter which restricts type.
coinF :: Format r (Coin -> r)
coinF = build

-- | Unwraps 'Coin'. It's called “unsafe” so that people wouldn't use it
-- willy-nilly if they want to sum coins or something. It's actually safe.
unsafeGetCoin :: Coin -> Word64
unsafeGetCoin = getCoin
{-# INLINE unsafeGetCoin #-}

-- | CoinPortion is some portion of Coin, it must be in [0 .. 1]. Main
-- usage of it is multiplication with Coin. Usually it's needed to
-- determine some threshold expressed as portion of total stake.
newtype CoinPortion = CoinPortion
    { getCoinPortion :: Double
    } deriving (Show, Ord, Eq)

-- | Make CoinPortion from Double. Caller must ensure that value is in [0 .. 1].
unsafeCoinPortion :: Double -> CoinPortion
unsafeCoinPortion x = assert (0 <= x && x <= 1) $ CoinPortion x
{-# INLINE unsafeCoinPortion #-}

----------------------------------------------------------------------------
-- Slotting
----------------------------------------------------------------------------

-- | Index of epoch.
newtype EpochIndex = EpochIndex
    { getEpochIndex :: Word64
    } deriving (Show, Eq, Ord, Num, Enum, Integral, Real, Generic, Hashable, Bounded, Typeable)

instance Buildable EpochIndex where
    build = bprint ("epoch #"%int)

-- instance Buildable (EpochIndex,EpochIndex) where
--     build = bprint ("epochIndices: "%pairF)

-- | Class for something that has 'EpochIndex'.
class HasEpochIndex a where
    epochIndexL :: Lens' a EpochIndex

-- | Index of slot inside a concrete epoch.
newtype LocalSlotIndex = LocalSlotIndex
    { getSlotIndex :: Word16
    } deriving (Show, Eq, Ord, Num, Enum, Ix, Integral, Real, Generic, Hashable, Buildable, Typeable)

-- | Slot is identified by index of epoch and local index of slot in
-- this epoch. This is a global index
data SlotId = SlotId
    { siEpoch :: !EpochIndex
    , siSlot  :: !LocalSlotIndex
    } deriving (Show, Eq, Ord, Generic, Typeable)

instance Buildable SlotId where
    build SlotId {..} =
        bprint (ords%" slot of "%ords%" epoch") siSlot siEpoch

-- | Specialized formatter for 'SlotId'.
slotIdF :: Format r (SlotId -> r)
slotIdF = build

-- | FlatSlotId is a flat version of SlotId
type FlatSlotId = Word64

-- | Represents SlotId or EpochIndex. Useful because genesis blocks
-- have only EpochIndex, while main blocks have SlotId.
newtype EpochOrSlot = EpochOrSlot
    { unEpochOrSlot :: Either EpochIndex SlotId
    } deriving (Show, Eq)

-- | Apply one of the function depending on content of EpochOrSlot.
epochOrSlot :: (EpochIndex -> a) -> (SlotId -> a) -> EpochOrSlot -> a
epochOrSlot f g = either f g . unEpochOrSlot

instance Ord EpochOrSlot where
    compare (EpochOrSlot e1) (EpochOrSlot e2) = case (e1,e2) of
        (Left s1, Left s2)                      -> compare s1 s2
        (Right s1, Left s2) | (siEpoch s1) < s2 -> LT
                            | otherwise         -> GT
        (Left s1, Right s2) | s1 > (siEpoch s2) -> GT
                            | otherwise         -> LT
        (Right s1, Right s2)
            | siEpoch s1 == siEpoch s2 -> siSlot s1 `compare` siSlot s2
            | otherwise -> siEpoch s1 `compare` siEpoch s2

instance Buildable EpochOrSlot where
    build = either Buildable.build Buildable.build . unEpochOrSlot

class HasEpochOrSlot a where
    _getEpochOrSlot :: a -> Either EpochIndex SlotId
    getEpochOrSlot :: a -> EpochOrSlot
    getEpochOrSlot = EpochOrSlot . _getEpochOrSlot
    epochOrSlotG :: Getter a EpochOrSlot
    epochOrSlotG = to getEpochOrSlot

-- | Timestamp is a number which represents some point in time. It is
-- used in MonadSlots and its meaning is up to implementation of this
-- type class. The only necessary knowledge is that difference between
-- timestamps is microsecond. Hence underlying type is Microsecond.
newtype Timestamp = Timestamp
    { getTimestamp :: Microsecond
    } deriving (Num, Eq, Ord, Enum, Real, Integral, Typeable)

----------------------------------------------------------------------------
-- Address
----------------------------------------------------------------------------

-- | Address is where you can send coins.
data Address
    = PubKeyAddress
          { addrKeyHash :: !(AddressHash PublicKey) }
    | ScriptAddress
          { addrScriptHash :: !(AddressHash Script) }
    deriving (Eq, Ord, Generic, Typeable)

-- | Stakeholder identifier (stakeholders are identified by their public keys)
type StakeholderId = AddressHash PublicKey

type AddressHash = AbstractHash Blake2s_224

----------------------------------------------------------------------------
-- ChainDifficulty
----------------------------------------------------------------------------

-- | Chain difficulty represents necessary effort to generate a
-- chain. In the simplest case it can be number of blocks in chain.
newtype ChainDifficulty = ChainDifficulty
    { getChainDifficulty :: Word64
    } deriving (Show, Eq, Ord, Num, Enum, Real, Integral, Generic, Buildable, Typeable)

-- | Type class for something that has 'ChainDifficulty'.
class HasDifficulty a where
    difficultyL :: Lens' a ChainDifficulty

----------------------------------------------------------------------------
-- Version
----------------------------------------------------------------------------

-- | Communication protocol version.
data BlockVersion = BlockVersion
    { bvMajor :: !Word16
    , bvMinor :: !Word16
    , bvAlt   :: !Word8
    } deriving (Eq, Generic, Ord, Typeable)

newtype ApplicationName = ApplicationName
    { getApplicationName :: Text
    } deriving (Eq, Ord, Show, Generic, Typeable, ToString, Hashable, Buildable)

-- | Numeric software version associated with ApplicationName.
type NumSoftwareVersion = Word32

-- | Software version.
data SoftwareVersion = SoftwareVersion
    { svAppName :: !ApplicationName
    , svNumber  :: !NumSoftwareVersion
    } deriving (Eq, Generic, Ord, Typeable)

----------------------------------------------------------------------------
-- HeaderHash
----------------------------------------------------------------------------

-- | 'Hash' of block header. This should be @Hash (BlockHeader ssc)@
-- but we don't want to have @ssc@ in 'HeaderHash' type.
type HeaderHash = Hash BlockHeaderStub
data BlockHeaderStub

-- | Specialized formatter for 'HeaderHash'.
headerHashF :: Format r (HeaderHash -> r)
headerHashF = build

-- | Class for something that has 'HeaderHash'.
class HasHeaderHash a where
    headerHash :: a -> HeaderHash
    headerHashG :: Getter a HeaderHash
    headerHashG = to headerHash
