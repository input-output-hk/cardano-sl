{-# LANGUAGE RankNTypes #-}
-- | Applications of runners to scenarios.

module Pos.Launcher.Launcher
       ( -- * Node launchers.
         runNodeReal
       ) where

import           Universum

import           Data.Reflection (give)
import           Mockable (Production)

import           Pos.Communication.Protocol (OutSpecs, WorkerSpec)
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Communication.Limits (HasUpdateLimits, HasTxpLimits,
                                           HasSscLimits, HasBlockLimits,
                                           UpdateLimits (..), TxpLimits (..),
                                           SscLimits (..), BlockLimits (..))
import           Pos.Configuration (HasNodeConfiguration)
import           Pos.Core (HasConfiguration, BlockVersionData (..))
import           Pos.Core.Coin (coinPortionToDouble)
import           Pos.DB.DB (initNodeDBs)
import           Pos.DB.Class (gsAdoptedBVData)
import           Pos.Infra.Configuration (HasInfraConfiguration)
import           Pos.Launcher.Param (NodeParams (..))
import           Pos.Launcher.Resource (NodeResources (..), bracketNodeResources)
import           Pos.Launcher.Runner (runRealMode)
import           Pos.Launcher.Scenario (runNode)
import           Pos.Ssc.Types (SscParams)
import           Pos.Txp (txpGlobalSettings)
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.WorkMode (EmptyMempoolExt, RealMode)

-----------------------------------------------------------------------------
-- Main launchers
-----------------------------------------------------------------------------

type HasLimits =
    ( HasUpdateLimits (RealMode EmptyMempoolExt)
    , HasSscLimits (RealMode EmptyMempoolExt)
    , HasTxpLimits (RealMode EmptyMempoolExt)
    , HasBlockLimits (RealMode EmptyMempoolExt)
    )

-- | Run full node in real mode.
runNodeReal
    :: ( HasConfigurations
       , HasCompileInfo
       )
    => NodeParams
    -> SscParams
    -> (HasLimits => ([WorkerSpec (RealMode EmptyMempoolExt)], OutSpecs))
    -> Production ()
runNodeReal np sscnp plugins = bracketNodeResources np sscnp txpGlobalSettings initNodeDBs action
  where
    action :: HasConfiguration => NodeResources EmptyMempoolExt (RealMode EmptyMempoolExt) -> Production ()
    action nr@NodeResources {..} = giveLimits $
        runRealMode
            nr
            (runNode nr plugins)

    -- Fulfill limits here. It's absolutely the wrong place to do it, but this
    -- will go away soon in favour of diffusion/logic split.
    updateLimits :: UpdateLimits (RealMode EmptyMempoolExt)
    updateLimits = UpdateLimits
        { updateVoteNumLimit = succ . ceiling . recip . coinPortionToDouble . bvdMpcThd <$> gsAdoptedBVData
        , maxProposalSize = bvdMaxProposalSize <$> gsAdoptedBVData
        }
    txpLimits :: TxpLimits (RealMode EmptyMempoolExt)
    txpLimits = TxpLimits
        { maxTxSize = bvdMaxTxSize <$> gsAdoptedBVData
        }
    sscLimits :: SscLimits (RealMode EmptyMempoolExt)
    sscLimits = SscLimits
        { commitmentsNumLimit = succ . ceiling . recip . coinPortionToDouble . bvdMpcThd <$> gsAdoptedBVData
        }
    blockLimits :: BlockLimits (RealMode EmptyMempoolExt)
    blockLimits = BlockLimits
        { maxBlockSize = bvdMaxBlockSize <$> gsAdoptedBVData
        , maxHeaderSize = bvdMaxHeaderSize <$> gsAdoptedBVData
        }
    giveLimits
      :: ( ( HasUpdateLimits (RealMode EmptyMempoolExt)
           , HasTxpLimits (RealMode EmptyMempoolExt)
           , HasBlockLimits (RealMode EmptyMempoolExt)
           , HasSscLimits (RealMode EmptyMempoolExt)
           ) => x
         ) -> x
    giveLimits x = give updateLimits $
        give sscLimits $
        give txpLimits $
        give blockLimits $ x
