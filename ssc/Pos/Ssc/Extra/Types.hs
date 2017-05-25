-- | Types which are related to SSC, but not to the concrete
-- implementation.

module Pos.Ssc.Extra.Types
       ( SscState (..)
       ) where

import           Universum

import           Pos.Ssc.Class.Types (Ssc (..))

-- | Whole state of SSC. Stored only in-memory by design.
data SscState ssc =
    SscState
    { sscGlobal :: !(TVar (SscGlobalState ssc))
    , sscLocal  :: !(TVar (SscLocalData ssc))
    }
