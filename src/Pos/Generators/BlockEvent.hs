module Pos.Generators.BlockEvent
       (
       -- * Block apply
         BlockApplyResult(..)
       , BlockEventApply(..)
       , beaInput
       , beaOutValid
       -- * Block rollback
       , RollbackCount(..)
       , BlockRollbackResult(..)
       , BlockEventRollback(..)
       , berInput
       , berOutValid
       -- * Block event sum
       , BlockEvent(..)
       ) where

import           Control.Lens            (makeLenses)
import           Numeric.Natural         (Natural)

import           Pos.Block.Core          (Block)
import           Pos.Ssc.GodTossing.Type (SscGodTossing)

data BlockApplyResult
    = BlockApplySuccess
    | BlockApplyFailure {- TODO: attach error info, such as:
                            * block is not a continuation of the chain
                            * block signature is invalid
                            * etc -}

data BlockEventApply = BlockEventApply
    { _beaInput    :: ![Block SscGodTossing]
    , _beaOutValid :: !BlockApplyResult
    }

makeLenses ''BlockEventApply

newtype RollbackCount = RollbackCount Natural

data BlockRollbackResult
    = BlockRollbackSuccess
    | BlockRollbackFailure {- TODO: attach error info, such as:
                                * not enough blocks to rollback
                                * rollback limit exceeded
                                * genesis block rollback
                                * etc -}

data BlockEventRollback = BlockEventRollback
    { _berInput    :: !RollbackCount
    , _berOutValid :: !BlockRollbackResult
    }

makeLenses ''BlockEventRollback

data BlockEvent
    = BlkEvApply BlockEventApply
    | BlkEvRollback BlockEventRollback
