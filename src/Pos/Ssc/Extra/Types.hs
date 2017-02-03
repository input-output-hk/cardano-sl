-- | Types which are related to SSC, but not to concrete SSC.

module Pos.Ssc.Extra.Types
       ( SscState (..)
       ) where

import qualified Control.Concurrent.STM as STM

import           Pos.Ssc.Class.Types    (Ssc (..))

-- | Whole state of SSC. Stored only in-memory by design.
data SscState ssc =
    SscState
    { sscGlobal :: !(STM.TVar (SscGlobalState ssc))
    , sscLocal  :: !(STM.TVar (SscLocalData ssc))
    }
