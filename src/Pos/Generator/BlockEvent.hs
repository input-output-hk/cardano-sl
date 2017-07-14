module Pos.Generator.BlockEvent
       (
       -- * Block apply
         BlockApplyResult(..)
       , BlockEventApply(..)
       , beaInput
       , beaOutValid
       -- * Block rollback
       , BlockRollbackResult(..)
       , BlockEventRollback(..)
       , berInput
       , berOutValid
       -- * Block event sum
       , BlockEvent(..)
       ) where

import           Control.Lens            (makeLenses)

import           Pos.Block.Types         (Blund)
import           Pos.Ssc.GodTossing.Type (SscGodTossing)

data BlockApplyResult
    = BlockApplySuccess
    | BlockApplyFailure {- TODO: attach error info, such as:
                            * block is not a continuation of the chain
                            * block signature is invalid
                            * etc -}

data BlockEventApply = BlockEventApply
    { _beaInput    :: ![Blund SscGodTossing]
    , _beaOutValid :: !BlockApplyResult
    }

makeLenses ''BlockEventApply

data BlockRollbackResult
    = BlockRollbackSuccess
    | BlockRollbackFailure {- TODO: attach error info, such as:
                                * not enough blocks to rollback
                                * rollback limit exceeded
                                * genesis block rollback
                                * etc -}

data BlockEventRollback = BlockEventRollback
    { _berInput    :: ![Blund SscGodTossing]
    , _berOutValid :: !BlockRollbackResult
    }

makeLenses ''BlockEventRollback

data BlockEvent
    = BlkEvApply BlockEventApply
    | BlkEvRollback BlockEventRollback
