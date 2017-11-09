-- | Parameters used by blockchain generator.

module Pos.Generator.Block.Param
       ( TxGenParams (..)
       , HasTxGenParams (..)
       , BlockGenParams (..)
       , HasBlockGenParams (..)
       ) where

import           Universum

import           Control.Lens.TH (makeClassy)
import           Data.Default (Default (..))
import qualified Data.Text.Buildable
import           Formatting (bprint, build, formatToString, int, (%))
import qualified Prelude
import           Serokell.Util (pairF)

import           Pos.AllSecrets (AllSecrets, HasAllSecrets (..))
import           Pos.Core (BlockCount, GenesisWStakeholders)
import           Pos.Txp (TxpGlobalSettings)

-- | Parameters for transactions payload generation.
data TxGenParams = TxGenParams
    { _tgpTxCountRange :: !(Word, Word)
    -- ^ Such (a, d), that there block will include some x ∈ [a, a+d)
    -- transactions. Set to (y,0) to disable tx generation.
    , _tgpMaxOutputs   :: !Word
    -- ^ Maximum number of tx outputs.
    }

makeClassy ''TxGenParams

instance Buildable TxGenParams where
    build TxGenParams {..} = do
        let (a,d) = _tgpTxCountRange
        bprint ("TxGenParams {\n"%
                "  tx count [from,to): "%pairF%"\n"%
                "  max outputs: "%int%"\n"%
                "}\n")
            (a,a+d)
            _tgpMaxOutputs

instance Show TxGenParams where
    show = formatToString build

instance Default TxGenParams where
    def = TxGenParams { _tgpTxCountRange = (0,5)
                      , _tgpMaxOutputs = 3
                      }

-- | Parameters for blockchain generation. Probably they come from the outside.
data BlockGenParams = BlockGenParams
    { _bgpSecrets           :: !AllSecrets
    -- ^ Secret data for the whole system.
    , _bgpBlockCount        :: !BlockCount
    -- ^ Number of blocks to generate.
    , _bgpTxGenParams       :: !TxGenParams
    -- ^ Transaction generation parameters.
    , _bgpInplaceDB         :: !Bool
    -- ^ Whether to extend existing DB.
    , _bgpSkipNoKey         :: !Bool
    -- ^ Skip block creation procedure if related leader key is not found.
    , _bgpGenStakeholders   :: !GenesisWStakeholders
    -- ^ Set of genesis stakeholders. This is needed to properly
    -- generate transaction payload.
    , _bgpTxpGlobalSettings :: !TxpGlobalSettings
    -- ^ Callbacks on verification, application and rollback blocks
    -- related to txp.
    }

makeClassy ''BlockGenParams

instance Buildable BlockGenParams where
    build BlockGenParams {..} =
        bprint ("BlockGenParams {\n"%
                "  secrets: "%build%"\n"%
                "  number of blocks: "%int%"\n"%
                "  tx gen params: "%build%"\n"%
                "}\n")
            _bgpSecrets
            _bgpBlockCount
            _bgpTxGenParams

instance HasAllSecrets BlockGenParams where
    allSecrets = bgpSecrets

instance HasTxGenParams BlockGenParams where
    txGenParams = bgpTxGenParams

instance Show BlockGenParams where
    show = formatToString build
