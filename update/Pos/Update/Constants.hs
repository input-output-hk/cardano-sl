{-# LANGUAGE TemplateHaskell #-}

module Pos.Update.Constants
       ( UpdateConstants(..)
       , updateConstants

       , ourAppName
       , lastKnownBlockVersion
       , curSoftwareVersion

       -- * Genesis constants
       , genesisBlockVersion
       , genesisSoftwareVersions
       , genesisBlockVersionData
       , genesisScriptVersion
       , genesisSlotDuration
       , genesisMaxBlockSize
       , genesisMaxHeaderSize
       , genesisMaxTxSize
       , genesisMpcThd
       , genesisHeavyDelThd
       , genesisUpdateVoteThd
       , genesisMaxUpdateProposalSize
       , genesisUpdateProposalThd
       , genesisUpdateImplicit
       , genesisUpdateSoftforkThd
       ) where

import           Universum

import           Data.Aeson                 (FromJSON (..), genericParseJSON)
import           Data.Tagged                (Tagged (..))
import           Data.Time.Units            (Millisecond, convertUnit)
import           Serokell.Aeson.Options     (defaultOptions)
import           Serokell.Data.Memory.Units (Byte)
import           Serokell.Util              (sec)

import           Pos.Core                   (ApplicationName, BlockVersion (..),
                                             CoinPortion, ScriptVersion,
                                             SoftwareVersion (..), mkApplicationName,
                                             unsafeCoinPortionFromDouble)
import           Pos.Update.Core.Types      (BlockVersionData (..))
import           Pos.Util.Config            (IsConfig (..), configParser,
                                             parseFromCslConfig)

----------------------------------------------------------------------------
-- Config itself
----------------------------------------------------------------------------

updateConstants :: UpdateConstants
updateConstants =
    case parseFromCslConfig configParser of
        Left err -> error (toText ("Couldn't parse update config: " ++ err))
        Right x  -> x

data UpdateConstants = UpdateConstants
    {
      -- | Name of this application.
      ccApplicationName              :: !Text
      -- | Length of slot in seconds
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
      -- | Portion of total stake such that if total stake of issuers of
      -- blocks with some block version is bigger than this portion, this
      -- block version is adopted.
    , ccGenesisUpdateSoftforkThd     :: !Double
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
      -- | Last known block version: major
    , ccLastKnownBVMajor             :: !Word16
      -- | Last known block version: minor
    , ccLastKnownBVMinor             :: !Word16
      -- | Last known block version: alt
    , ccLastKnownBVAlt               :: !Word8
      -- | Application version
    , ccApplicationVersion           :: !Word32
    }
    deriving (Show, Generic)

instance FromJSON UpdateConstants where
    parseJSON = genericParseJSON defaultOptions

instance IsConfig UpdateConstants where
    configPrefix = Tagged Nothing

----------------------------------------------------------------------------
-- Various constants
----------------------------------------------------------------------------

-- | Name of our application.
ourAppName :: ApplicationName
ourAppName =
    either (error . mappend "Failed to init our application name: ") identity $
    mkApplicationName $ ccApplicationName updateConstants

-- | Last block version application is aware of.
lastKnownBlockVersion :: BlockVersion
lastKnownBlockVersion = BlockVersion (ccLastKnownBVMajor updateConstants)
                                     (ccLastKnownBVMinor updateConstants)
                                     (ccLastKnownBVAlt updateConstants)

-- | Version of application (code running)
curSoftwareVersion :: SoftwareVersion
curSoftwareVersion = SoftwareVersion ourAppName
                                     (ccApplicationVersion updateConstants)

----------------------------------------------------------------------------
-- Genesis constants
----------------------------------------------------------------------------

-- | BlockVersion used at the very beginning.
genesisBlockVersion :: BlockVersion
genesisBlockVersion =
    BlockVersion
    { bvMajor = 0
    , bvMinor = 0
    , bvAlt = 0
    }

-- | Software Versions
genesisSoftwareVersions :: [SoftwareVersion]
genesisSoftwareVersions = map f genesisAppNames
  where
    f (nameStr, Left err) =
        error $
        "Failed to create ApplicationName for " <> nameStr <> ": " <> err
    f (_, Right appName) = SoftwareVersion {svAppName = appName, svNumber = 0}

genesisAppNames :: [(Text, Either Text ApplicationName)]
genesisAppNames = map f ["cardano-sl", "csl-daedalus"]
  where
    f name = (name, mkApplicationName name)

-- | 'BlockVersionData' for genesis 'BlockVersion'.
genesisBlockVersionData :: BlockVersionData
genesisBlockVersionData =
    BlockVersionData
    { bvdScriptVersion = genesisScriptVersion
    , bvdSlotDuration = genesisSlotDuration
    , bvdMaxBlockSize = genesisMaxBlockSize
    , bvdMaxHeaderSize = genesisMaxHeaderSize
    , bvdMaxTxSize = genesisMaxTxSize
    , bvdMaxProposalSize = genesisMaxUpdateProposalSize
    , bvdMpcThd = genesisMpcThd
    , bvdHeavyDelThd = genesisHeavyDelThd
    , bvdUpdateVoteThd = genesisUpdateVoteThd
    , bvdUpdateProposalThd = genesisUpdateProposalThd
    , bvdUpdateImplicit = genesisUpdateImplicit
    , bvdUpdateSoftforkThd = genesisUpdateSoftforkThd
    }

-- | ScriptVersion used at the very beginning
genesisScriptVersion :: ScriptVersion
genesisScriptVersion = 0

-- | Initial length of slot.
genesisSlotDuration :: Millisecond
genesisSlotDuration = convertUnit . sec $
    ccGenesisSlotDurationSec updateConstants

-- | Initial block size limit.
genesisMaxBlockSize :: Byte
genesisMaxBlockSize = ccGenesisMaxBlockSize updateConstants

-- | See 'ccGenesisUpdateVoteThd'.
genesisUpdateVoteThd :: CoinPortion
genesisUpdateVoteThd = unsafeCoinPortionFromDouble $
    ccGenesisUpdateVoteThd updateConstants

-- | See 'ccGenesisMaxUpdateProposalSize'.
genesisMaxUpdateProposalSize :: Byte
genesisMaxUpdateProposalSize =
    ccGenesisMaxUpdateProposalSize updateConstants

-- | See 'ccGenesisUpdateProposalThd'.
genesisUpdateProposalThd :: CoinPortion
genesisUpdateProposalThd = unsafeCoinPortionFromDouble $
    ccGenesisUpdateProposalThd updateConstants

-- | See 'ccGenesisUpdateImplicit'.
genesisUpdateImplicit :: Integral i => i
genesisUpdateImplicit = fromIntegral $
    ccGenesisUpdateImplicit updateConstants

-- | See 'ccGenesisUpdateSoftforkThd'.
genesisUpdateSoftforkThd :: CoinPortion
genesisUpdateSoftforkThd = unsafeCoinPortionFromDouble $
    ccGenesisUpdateSoftforkThd updateConstants

-- | Maximum size of a block header (in bytes)
genesisMaxHeaderSize :: Byte
genesisMaxHeaderSize = ccGenesisMaxHeaderSize updateConstants

-- | See 'Pos.CompileConfig.ccGenesisMaxTxSize'.
genesisMaxTxSize :: Byte
genesisMaxTxSize = ccGenesisMaxTxSize updateConstants

-- | See 'Pos.CompileConfig.ccGenesisMpcThd'.
genesisMpcThd :: CoinPortion
genesisMpcThd = unsafeCoinPortionFromDouble $
    ccGenesisMpcThd updateConstants

-- | See 'Pos.CompileConfig.ccGenesisHeavyDelThd'.
genesisHeavyDelThd :: CoinPortion
genesisHeavyDelThd = unsafeCoinPortionFromDouble $
    ccGenesisHeavyDelThd updateConstants

----------------------------------------------------------------------------
-- Asserts
----------------------------------------------------------------------------

{- I'm just going to move them somewhere at some point,
   because they won't work in this module
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

staticAssert
    (ccGenesisMpcThd updateConstants >= 0 &&
     ccGenesisMpcThd updateConstants < 1)
    "genesisMpcThd is not in range [0, 1)"

staticAssert
    (ccGenesisUpdateVoteThd updateConstants >= 0 &&
     ccGenesisUpdateVoteThd updateConstants < 1)
    "genesisUpdateVoteThd is not in range [0, 1)"

staticAssert
    (ccGenesisHeavyDelThd updateConstants >= 0 &&
     ccGenesisHeavyDelThd updateConstants < 1)
    "genesisHeavyDelThd is not in range [0, 1)"

staticAssert
    (ccGenesisUpdateProposalThd updateConstants > 0 &&
     ccGenesisUpdateProposalThd updateConstants < 1)
    "genesisUpdateProposalThd is not in range (0, 1)"

staticAssert
    (ccGenesisUpdateSoftforkThd updateConstants > 0 &&
     ccGenesisUpdateSoftforkThd updateConstants < 1)
    "genesisUpdateSoftforkThd is not in range (0, 1)"

staticAssert
    (isJust $ mkApplicationName $ ccApplicationName updateConstants)
    "it's sad, because ourAppName will be `error' sadly"

staticAssert
    (all (isRight . snd) $ genesisAppNames)
    "it's sad too, I guess you realize it"

-}
