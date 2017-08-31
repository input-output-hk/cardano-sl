{-# LANGUAGE CPP #-}

-- | Raw constants which we want to have in 'core'. They have simpler
-- types than constants from 'Typed' module. It's done to avoid cyclic
-- dependencies. To be more precise, this module doesn't import
-- 'Pos.Core.Types', while 'Typed' module does.

module Pos.Core.Constants.Raw
       (
       -- * Non-configurable constants
         sharedSeedLength
       , genesisHash

       -- * The config structure
       , CoreConfig(..)
       , coreConfig

       -- * Constants
       , isDevelopment
       , protocolMagic
       , staticSysStartRaw
       , genesisKeysN
       , memPoolLimitRatio

       , genesisBinSuffix

       , nonCriticalCQBootstrap
       , criticalCQBootstrap
       , nonCriticalCQ
       , criticalCQ
       , criticalForkThreshold

       , webLoggingEnabled
       ) where

import           Universum

import           Data.Aeson                 (FromJSON (..), genericParseJSON)
import qualified Data.Aeson.Types           as A
import           Data.Tagged                (Tagged (..))
import           Data.Time.Units            (Microsecond)
import           Serokell.Aeson.Options     (defaultOptions)
import           Serokell.Data.Memory.Units (Byte)
import           Serokell.Util              (sec)

import           Pos.Core.Fee               (TxFeePolicy)
import           Pos.Core.Fee.Config        (ConfigOf (..))
import           Pos.Crypto.Hashing         (Hash, unsafeHash)
import           Pos.Util.Config            (IsConfig (..), configParser,
                                             parseFromCslConfig)
import           Pos.Util.Util              ()

----------------------------------------------------------------------------
-- Constants which are not configurable
----------------------------------------------------------------------------

-- | Length of shared seed.
sharedSeedLength :: Integral a => a
sharedSeedLength = 32

-- | Predefined 'Hash' of the parent of the 0-th genesis block (which
-- is the only block without a real parent).
genesisHash :: Hash a
genesisHash = unsafeHash @Text "patak"
{-# INLINE genesisHash #-}

----------------------------------------------------------------------------
-- Config itself
----------------------------------------------------------------------------

data CoreConfig = CoreConfig
    {
      -- | Security parameter from paper
      ccK                            :: !Int
    , -- | Magic constant for separating real/testnet
      ccProtocolMagic                :: !Int32
    , -- | Start time of network (in @Production@ running mode). If set to
      -- zero, then running time is 2 minutes after build.
      ccProductionNetworkStartTime   :: !Int
    , -- | Number of pre-generated keys
      ccGenesisN                     :: !Int
      -- | Size of mem pool will be limited by this value muliplied by block
      -- size limit.
    , ccMemPoolLimitRatio            :: !Word
      -- | Suffix for genesis.bin files
    , ccGenesisBinSuffix             :: ![Char]

       -- Genesis block version data

      -- | Genesis length of slot in seconds.
    , ccGenesisSlotDurationSec       :: !Int
      -- | Portion of total stake necessary to vote for or against update.
    , ccGenesisUpdateVoteThd         :: !Double
      -- | Maximum update proposal size in bytes
    , ccGenesisMaxUpdateProposalSize :: !Byte
      -- | Portion of total stake such that block containing UpdateProposal
      -- must contain positive votes for this proposal from stakeholders
      -- owning at least this amount of stake.
    , ccGenesisUpdateProposalThd     :: !Double
      -- | Number of slots after which update is implicitly approved unless
      -- it has more negative votes than positive.
    , ccGenesisUpdateImplicit        :: !Word
      -- | The following three values define 'SoftforkRule', see the
      -- documentation of this type for more detail.
    , ccGenesisSoftforkInit          :: !Double
      -- | See 'SoftforkRule'.
    , ccGenesisSoftforkMin           :: !Double
      -- | See 'SoftforkRule'.
    , ccGenesisSoftforkDec           :: !Double
    , ccGenesisTxFeePolicy           :: !(ConfigOf TxFeePolicy)
    , ccGenesisUnlockStakeEpoch      :: !Word64
      -- | Maximum block size in bytes
    , ccGenesisMaxBlockSize          :: !Byte
      -- | Maximum block header size in bytes
    , ccGenesisMaxHeaderSize         :: !Byte
      -- | Maximum tx size in bytes
    , ccGenesisMaxTxSize             :: !Byte
      -- | Threshold for heavyweight delegation
    , ccGenesisHeavyDelThd           :: !Double
      -- | Eligibility threshold for MPC
    , ccGenesisMpcThd                :: !Double

       -- Chain quality thresholds and other constants to detect
       -- suspicious things.

      -- | If chain quality in bootstrap era is less than this value,
      -- non critical misbehavior will be reported.
    , ccNonCriticalCQBootstrap       :: !Double
      -- | If chain quality in bootstrap era is less than this value,
      -- critical misbehavior will be reported.
    , ccCriticalCQBootstrap          :: !Double
      -- | If chain quality after bootstrap era is less than this
      -- value, non critical misbehavior will be reported.
    , ccNonCriticalCQ                :: !Double
      -- | If chain quality after bootstrap era is less than this
      -- value, critical misbehavior will be reported.
    , ccCriticalCQ                   :: !Double
      -- | Number of blocks such that if so many blocks are rolled
      -- back, it requires immediate reaction.
    , ccCriticalForkThreshold        :: !Int

       -- Web settings

      -- | Whether incoming requests logging should be performed by web
      -- part
    , ccWebLoggingEnabled            :: !Bool
    }
    deriving (Show, Generic)

-- | Suffix for genesis.bin files
genesisBinSuffix :: [Char]
genesisBinSuffix = ccGenesisBinSuffix coreConfig

coreConfig :: CoreConfig
coreConfig =
    case parseFromCslConfig configParser of
        Left err -> error (toText ("Couldn't parse core config: " ++ err))
        Right x  -> x

instance FromJSON CoreConfig where
    parseJSON = checkConstants <=< genericParseJSON defaultOptions

instance IsConfig CoreConfig where
    configPrefix = Tagged Nothing

-- | Check invariants
checkConstants :: CoreConfig -> A.Parser CoreConfig
checkConstants cs@CoreConfig{..} = do
    let check :: [a -> Bool] -> a -> Bool
        check ps x = all ($ x) ps
    unless (check [(>= 0), (< 1)] ccGenesisMpcThd) $
        fail "CoreConfig: genesisMpcThd is not in range [0, 1)"
    unless (check [(>= 0), (< 1)] ccGenesisUpdateVoteThd) $
        fail "CoreConfig: genesisUpdateVoteThd is not in range [0, 1)"
    unless (check [(>= 0), (< 1)] ccGenesisHeavyDelThd) $
        fail "CoreConfig: genesisHeavyDelThd is not in range [0, 1)"
    unless (check [(> 0), (< 1)] ccGenesisUpdateProposalThd) $
        fail "CoreConfig: genesisUpdateProposalThd is not in range (0, 1)"
    unless (check [(> 0), (< 1)] ccGenesisSoftforkInit) $
        fail "CoreConfig: genesisSoftforkInit is not in range (0, 1)"
    unless (check [(> 0), (< 1)] ccGenesisSoftforkMin) $
        fail "CoreConfig: genesisSoftforkMin is not in range (0, 1)"
    unless (check [(> 0), (< 1)] ccGenesisSoftforkDec) $
        fail "CoreConfig: genesisSoftforkDec is not in range (0, 1)"
    pure cs

----------------------------------------------------------------------------
-- Constants taken from the config
----------------------------------------------------------------------------

-- | @True@ if current mode is 'Development'.
isDevelopment :: Bool
#ifdef DEV_MODE
isDevelopment = True
#else
isDevelopment = False
#endif

-- | System start time embedded into binary.
staticSysStartRaw :: Microsecond
staticSysStartRaw
    | isDevelopment = error "System start time should be passed \
                              \as a command line argument in dev mode."
    | otherwise     =
          sec $ ccProductionNetworkStartTime coreConfig

-- | Protocol magic constant. Is put to block serialized version to
-- distinguish testnet and realnet (for example, possible usages are
-- wider).
protocolMagic :: Int32
protocolMagic = fromIntegral . ccProtocolMagic $ coreConfig

-- | Number of pre-generated keys
genesisKeysN :: Integral i => i
genesisKeysN = fromIntegral . ccGenesisN $ coreConfig

-- | Size of mem pool will be limited by this value muliplied by block
-- size limit.
memPoolLimitRatio :: Integral i => i
memPoolLimitRatio = fromIntegral . ccMemPoolLimitRatio $ coreConfig

-- | If chain quality in bootstrap era is less than this value,
-- non critical misbehavior will be reported.
nonCriticalCQBootstrap :: Double
nonCriticalCQBootstrap = ccNonCriticalCQBootstrap coreConfig

-- | If chain quality in bootstrap era is less than this value,
-- critical misbehavior will be reported.
criticalCQBootstrap :: Double
criticalCQBootstrap = ccCriticalCQBootstrap coreConfig

-- | If chain quality after bootstrap era is less than this
-- value, non critical misbehavior will be reported.
nonCriticalCQ :: Double
nonCriticalCQ = ccNonCriticalCQ coreConfig

-- | If chain quality after bootstrap era is less than this
-- value, critical misbehavior will be reported.
criticalCQ :: Double
criticalCQ = ccCriticalCQ coreConfig

-- | If chain quality after bootstrap era is less than this
-- value, critical misbehavior will be reported.
criticalForkThreshold :: Integral i => i
criticalForkThreshold = fromIntegral . ccCriticalForkThreshold $ coreConfig

-- | Web logging might be disabled for security concerns.
webLoggingEnabled :: Bool
webLoggingEnabled = ccWebLoggingEnabled coreConfig
