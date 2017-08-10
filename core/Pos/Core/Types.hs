{-# LANGUAGE ScopedTypeVariables #-}

-- | Core types. TODO: we need to have a meeting, come up with project
-- structure and follow it.

module Pos.Core.Types
       (
       -- * Address
         Address (..)
       , _RedeemAddress
       , AddrPkAttrs (..)
       , AddressHash
       , StakeholderId
       , StakesMap

       , Timestamp (..)
       , TimeDiff (..)

       -- * ChainDifficulty
       , ChainDifficulty (..)

       -- * Version
       , ApplicationName
       , getApplicationName
       , BlockVersion (..)
       , NumSoftwareVersion
       , SoftwareVersion (..)
       , applicationNameMaxLength
       , mkApplicationName

       -- * Update system
       , SoftforkRule (..)
       , BlockVersionData (..)

       -- * HeaderHash related types and functions
       , BlockHeaderStub
       , HeaderHash
       , headerHashF

       , ProxySigLight
       , ProxySKLight
       , ProxySigHeavy
       , ProxySKHeavy

       , SharedSeed (..)
       , SlotLeaders

       -- * Coin
       , Coin
       , CoinPortion
       , coinF
       , unsafeGetCoin
       , getCoinPortion
       , mkCoin
       , coinPortionDenominator
       , mkCoinPortion
       , unsafeCoinPortionFromDouble
       , maxCoinVal

       -- * Slotting
       , EpochIndex (..)
       , FlatSlotId
       , LocalSlotIndex (getSlotIndex)
       , mkLocalSlotIndex
       , addLocalSlotIndex
       , SlotId (..)
       , siEpochL
       , siSlotL
       , slotIdF
       , EpochOrSlot (..)

       -- * Scripting
       , Script(..)
       , Script_v0
       , ScriptVersion

       -- * Newtypes
       -- ** for amounts
       , BlockCount(..)
       , SlotCount(..)
       ) where

import           Universum

import           Control.Lens               (makeLensesFor, makePrisms)
import           Control.Monad.Except       (MonadError (throwError))
import           Crypto.Hash                (Blake2b_224)
import           Data.Char                  (isAscii)
import           Data.Data                  (Data)
import           Data.Default               (Default (..))
import           Data.Hashable              (Hashable)
import           Data.Ix                    (Ix)
import qualified Data.Text                  as T
import qualified Data.Text.Buildable        as Buildable
import           Data.Time.Units            (Millisecond)
import           Formatting                 (Format, bprint, build, formatToString, int,
                                             ords, shown, stext, (%))
import qualified PlutusCore.Program         as PLCore
import qualified Prelude
import           Serokell.AcidState         ()
import           Serokell.Data.Memory.Units (Byte)
import           Serokell.Util.Base16       (formatBase16)
import           System.Random              (Random (..))

import           Pos.Core.Constants.Raw     (epochSlotsRaw)
import           Pos.Core.Fee               (TxFeePolicy)
import           Pos.Core.Timestamp         (TimeDiff (..), Timestamp (..))
import           Pos.Crypto                 (AbstractHash, HDAddressPayload, Hash,
                                             ProxySecretKey, ProxySignature, PublicKey,
                                             RedeemPublicKey)
import           Pos.Data.Attributes        (Attributes)

----------------------------------------------------------------------------
-- Address
----------------------------------------------------------------------------

-- | Hash used to identify address.
type AddressHash = AbstractHash Blake2b_224

-- | Stakeholder identifier (stakeholders are identified by their public keys)
type StakeholderId = AddressHash PublicKey

-- | Address is where you can send coins.
data Address
    = PubKeyAddress
          { addrKeyHash      :: !(AddressHash PublicKey)
          , addrPkAttributes :: !(Attributes AddrPkAttrs) }
    | ScriptAddress
          { addrScriptHash :: !(AddressHash Script) }
    | RedeemAddress
          { addrRedeemKeyHash :: !(AddressHash RedeemPublicKey) }
    | UnknownAddressType !Word8 !ByteString
    deriving (Eq, Ord, Generic, Typeable, Show)

instance NFData Address

newtype AddrPkAttrs = AddrPkAttrs
    { addrPkDerivationPath :: Maybe HDAddressPayload
    } deriving (Eq, Ord, Show, Generic, Typeable, NFData)

instance Default AddrPkAttrs where
    def = AddrPkAttrs Nothing

-- | A mapping between stakeholders and they stakes.
type StakesMap = HashMap StakeholderId Coin

----------------------------------------------------------------------------
-- ChainDifficulty
----------------------------------------------------------------------------

-- | Chain difficulty represents necessary effort to generate a
-- chain. In the simplest case it can be number of blocks in chain.
newtype ChainDifficulty = ChainDifficulty
    { getChainDifficulty :: BlockCount
    } deriving (Show, Eq, Ord, Num, Enum, Real, Integral, Generic, Buildable, Typeable, NFData)

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
    } deriving (Eq, Ord, Show, Generic, Typeable, ToString, Hashable, Buildable, NFData)

-- | Smart constructor of 'ApplicationName'.
mkApplicationName :: MonadFail m => Text -> m ApplicationName
mkApplicationName appName
    | length appName > applicationNameMaxLength =
        fail "ApplicationName: too long string passed"
    | T.any (not . isAscii) appName =
        fail "ApplicationName: not ascii string passed"
    | otherwise = pure $ ApplicationName appName

applicationNameMaxLength :: Integral i => i
applicationNameMaxLength = 12

-- | Numeric software version associated with ApplicationName.
type NumSoftwareVersion = Word32

-- | Software version.
data SoftwareVersion = SoftwareVersion
    { svAppName :: !ApplicationName
    , svNumber  :: !NumSoftwareVersion
    } deriving (Eq, Generic, Ord, Typeable)

instance Buildable SoftwareVersion where
    build SoftwareVersion {..} =
        bprint (stext % ":" % int) (getApplicationName svAppName) svNumber

instance Show SoftwareVersion where
    show = toString . pretty

instance Show BlockVersion where
    show BlockVersion {..} =
        intercalate "." [show bvMajor, show bvMinor, show bvAlt]

instance Buildable BlockVersion where
    build = bprint shown

instance Hashable SoftwareVersion
instance Hashable BlockVersion

instance NFData BlockVersion
instance NFData SoftwareVersion

----------------------------------------------------------------------------
-- Values updatable by update system
----------------------------------------------------------------------------

-- | Values defining softfork resolution rule.
-- If a proposal is adopted at the 's'-th epoch, softfork resolution threshold
-- at the 't'-th epoch will be
-- 'max spMinThd (spInitThd - (t - s) * spThdDecrement)'.
--
-- Softfork resolution threshold is the portion of total stake such
-- that if total stake of issuers of blocks with some block version is
-- greater than this portion, this block version becomes adopted.
data SoftforkRule = SoftforkRule
    { srInitThd      :: !CoinPortion
    -- ^ Initial threshold (right after proposal is confirmed).
    , srMinThd       :: !CoinPortion
    -- ^ Minimal threshold (i. e. threshold can't become less than
    -- this one).
    , srThdDecrement :: !CoinPortion
    -- ^ Theshold will be decreased by this value after each epoch.
    } deriving (Show, Eq, Generic)

-- | Data which is associated with 'BlockVersion'.
data BlockVersionData = BlockVersionData
    { bvdScriptVersion     :: !ScriptVersion
    , bvdSlotDuration      :: !Millisecond
    , bvdMaxBlockSize      :: !Byte
    , bvdMaxHeaderSize     :: !Byte
    , bvdMaxTxSize         :: !Byte
    , bvdMaxProposalSize   :: !Byte
    , bvdMpcThd            :: !CoinPortion
    , bvdHeavyDelThd       :: !CoinPortion
    , bvdUpdateVoteThd     :: !CoinPortion
    , bvdUpdateProposalThd :: !CoinPortion
    , bvdUpdateImplicit    :: !FlatSlotId
    , bvdSoftforkRule      :: !SoftforkRule
    , bvdTxFeePolicy       :: !TxFeePolicy
    , bvdUnlockStakeEpoch  :: !EpochIndex
    } deriving (Show, Eq, Generic, Typeable)

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

----------------------------------------------------------------------------
-- Proxy signatures and delegation
----------------------------------------------------------------------------

-- | Proxy signature, that holds a pair of epoch indices. Block is
-- valid if its epoch index is inside this range.
type ProxySigLight a = ProxySignature (EpochIndex, EpochIndex) a

-- | Same alias for the proxy secret key (see 'ProxySigLight').
type ProxySKLight = ProxySecretKey (EpochIndex, EpochIndex)

-- | Simple proxy signature without ttl/epoch index
-- constraints. 'EpochIndex' inside is needed for replay attack
-- prevention (it should match epoch of the block psk is announced
-- in).
type ProxySigHeavy a = ProxySignature EpochIndex a

-- | Heavy delegation psk.
type ProxySKHeavy = ProxySecretKey EpochIndex

----------------------------------------------------------------------------
-- SSC. It means shared seed computation, btw
----------------------------------------------------------------------------

-- | This is a shared seed used for follow-the-satoshi. This seed is
-- randomly generated by each party and eventually they agree on the
-- same value.
newtype SharedSeed = SharedSeed
    { getSharedSeed :: ByteString
    } deriving (Show, Eq, Ord, Generic, NFData, Typeable)

instance Buildable SharedSeed where
    build = formatBase16 . getSharedSeed

-- | 'NonEmpty' list of slot leaders.
type SlotLeaders = NonEmpty StakeholderId

----------------------------------------------------------------------------
-- Coin
----------------------------------------------------------------------------

-- | Coin is the least possible unit of currency.
newtype Coin = Coin
    { getCoin :: Word64
    } deriving (Show, Ord, Eq, Generic, Hashable, Data, NFData)

instance Buildable Coin where
    build (Coin n) = bprint (int%" coin(s)") n

instance Bounded Coin where
    minBound = Coin 0
    maxBound = Coin maxCoinVal

-- | Maximal possible value of 'Coin'.
maxCoinVal :: Word64
maxCoinVal = 45000000000000000

-- | Make Coin from Word64.
mkCoin :: Word64 -> Coin
mkCoin c
    | c <= maxCoinVal = Coin c
    | otherwise       = error $ "mkCoin: " <> show c <> " is too large"
{-# INLINE mkCoin #-}

-- | Coin formatter which restricts type.
coinF :: Format r (Coin -> r)
coinF = build

-- | Unwraps 'Coin'. It's called “unsafe” so that people wouldn't use it
-- willy-nilly if they want to sum coins or something. It's actually safe.
unsafeGetCoin :: Coin -> Word64
unsafeGetCoin = getCoin
{-# INLINE unsafeGetCoin #-}

-- | CoinPortion is some portion of Coin; it is interpreted as a fraction
-- with denominator of 'coinPortionDenominator'. The numerator must be in the
-- interval of [0, coinPortionDenominator].
--
-- Usually 'CoinPortion' is used to determine some threshold expressed as
-- portion of total stake.
--
-- To multiply a coin portion by 'Coin', use 'applyCoinPortionDown' (when
-- calculating number of coins) or 'applyCoinPortionUp' (when calculating a
-- threshold).
newtype CoinPortion = CoinPortion
    { getCoinPortion :: Word64
    } deriving (Show, Ord, Eq, Generic, Typeable, NFData)

-- | Denominator used by 'CoinPortion'.
coinPortionDenominator :: Word64
coinPortionDenominator = (10 :: Word64) ^ (15 :: Word64)

-- | Make 'CoinPortion' from 'Word64' checking whether it is not greater
-- than 'coinPortionDenominator'.
mkCoinPortion
    :: MonadFail m
    => Word64 -> m CoinPortion
mkCoinPortion x
    | x <= coinPortionDenominator = pure $ CoinPortion x
    | otherwise = fail err
  where
    err =
        formatToString
            ("mkCoinPortion: value is greater than coinPortionDenominator: "
            %int) x

-- | Make CoinPortion from Double. Caller must ensure that value is in
-- [0..1]. Internally 'CoinPortion' stores 'Word64' which is divided by
-- 'coinPortionDenominator' to get actual value. So some rounding may take
-- place.
unsafeCoinPortionFromDouble :: Double -> CoinPortion
unsafeCoinPortionFromDouble x
    | 0 <= x && x <= 1 = CoinPortion v
    | otherwise = error "unsafeCoinPortionFromDouble: double not in [0, 1]"
  where
    v = round $ realToFrac coinPortionDenominator * x
{-# INLINE unsafeCoinPortionFromDouble #-}

----------------------------------------------------------------------------
-- Slotting
----------------------------------------------------------------------------

-- | Index of epoch.
newtype EpochIndex = EpochIndex
    { getEpochIndex :: Word64
    } deriving (Show, Eq, Ord, Num, Enum, Ix, Integral, Real, Generic, Hashable, Bounded, Typeable, NFData)

instance Buildable EpochIndex where
    build = bprint ("epoch #"%int)

-- instance Buildable (EpochIndex,EpochIndex) where
--     build = bprint ("epochIndices: "%pairF)

-- | Index of slot inside a concrete epoch.
newtype LocalSlotIndex = LocalSlotIndex
    { getSlotIndex :: Word16
    } deriving (Show, Eq, Ord, Ix, Generic, Hashable, Buildable, Typeable, NFData)

instance Bounded LocalSlotIndex where
    minBound = LocalSlotIndex 0
    maxBound = LocalSlotIndex (epochSlotsRaw - 1)

instance Enum LocalSlotIndex where
    toEnum i | i >= epochSlotsRaw = error "toEnum @LocalSlotIndex: greater than maxBound"
             | i < 0 = error "toEnum @LocalSlotIndex: less than minBound"
             | otherwise = LocalSlotIndex (fromIntegral i)
    fromEnum = fromIntegral . getSlotIndex

instance Random LocalSlotIndex where
    random = randomR (minBound, maxBound)
    randomR (LocalSlotIndex lo, LocalSlotIndex hi) g =
        let (r, g') = randomR (lo, hi) g
        in  (LocalSlotIndex r, g')

mkLocalSlotIndex :: MonadError Text m => Word16 -> m LocalSlotIndex
mkLocalSlotIndex idx
    | idx < epochSlotsRaw = pure (LocalSlotIndex idx)
    | otherwise =
        throwError $
        "local slot is greater than or equal to the number of slots in epoch: " <>
        show idx

-- | Shift slot index by given amount, and return 'Nothing' if it has
-- overflowed past 'epochSlots'.
addLocalSlotIndex :: SlotCount -> LocalSlotIndex -> Maybe LocalSlotIndex
addLocalSlotIndex x (LocalSlotIndex i)
    | s < epochSlotsRaw = Just (LocalSlotIndex (fromIntegral s))
    | otherwise         = Nothing
  where
    s :: Word64
    s = fromIntegral x + fromIntegral i

-- | Slot is identified by index of epoch and local index of slot in
-- this epoch. This is a global index
data SlotId = SlotId
    { siEpoch :: !EpochIndex
    , siSlot  :: !LocalSlotIndex
    } deriving (Show, Eq, Ord, Generic, Typeable)

instance Buildable SlotId where
    build SlotId {..} =
        bprint (ords%" slot of "%ords%" epoch") (getSlotIndex siSlot) siEpoch

-- | Specialized formatter for 'SlotId'.
slotIdF :: Format r (SlotId -> r)
slotIdF = build

-- | FlatSlotId is a flat version of SlotId
type FlatSlotId = Word64

-- | Represents SlotId or EpochIndex. Useful because genesis blocks
-- have only EpochIndex, while main blocks have SlotId.
newtype EpochOrSlot = EpochOrSlot
    { unEpochOrSlot :: Either EpochIndex SlotId
    } deriving (Show, Eq, Generic, NFData)

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

instance NFData SlotId

----------------------------------------------------------------------------
-- Script
----------------------------------------------------------------------------

-- | Version of script
type ScriptVersion = Word16

-- | A script for inclusion into a transaction.
data Script = Script
    { scrVersion :: ScriptVersion -- ^ Version
    , scrScript  :: ByteString   -- ^ Serialized script
    } deriving (Eq, Show, Generic, Typeable)

instance NFData Script
instance Hashable Script

instance Buildable Script where
    build Script{..} = bprint ("<script v"%int%">") scrVersion

-- | Deserialized script (i.e. an AST), version 0.
type Script_v0 = PLCore.Program

----------------------------------------------------------------------------
-- Newtypes
----------------------------------------------------------------------------

newtype BlockCount = BlockCount {getBlockCount :: Word64}
    deriving (Eq, Ord, Num, Real, Integral, Enum, Read, Show,
              Buildable, Generic, Typeable, NFData, Hashable, Random)

newtype SlotCount = SlotCount {getSlotCount :: Word64}
    deriving (Eq, Ord, Num, Real, Integral, Enum, Read, Show,
              Buildable, Generic, Typeable, NFData, Hashable, Random)

----------------------------------------------------------------------------
-- Template Haskell invocations, banished to the end of the module because
-- we don't want to topsort the whole module
----------------------------------------------------------------------------

flip makeLensesFor ''SlotId [
    ("siEpoch", "siEpochL"),
    ("siSlot" , "siSlotL") ]

makePrisms ''Address
