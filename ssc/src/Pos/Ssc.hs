-- | Re-exports of SSC modules.
--
-- We implement SSC as a coin tossing protocol with guaranteed output
-- delivery. Nodes exchange commitments, openings, and shares, and in the
-- end arrive at a shared seed.
--
-- See https://eprint.iacr.org/2016/889.pdf (“A Provably Secure
-- Proof-of-Stake Blockchain Protocol”), section 4 for more details.

module Pos.Ssc
       ( module Pos.Ssc.Error
       , module Pos.Ssc.Logic
       , module Pos.Ssc.Toss
       , module Pos.Ssc.Base
       , module Pos.Ssc.Behavior
       , module Pos.Ssc.Configuration
       , module Pos.Ssc.DB
       , module Pos.Ssc.Functions
       , module Pos.Ssc.Mem
       , module Pos.Ssc.Message
       , module Pos.Ssc.SecretStorage
       , module Pos.Ssc.Seed
       , module Pos.Ssc.State
       , module Pos.Ssc.Types
       , module Pos.Ssc.VssCertData
       ) where

import           Pos.Ssc.Base
import           Pos.Ssc.Behavior
import           Pos.Ssc.Configuration
import           Pos.Ssc.DB
import           Pos.Ssc.Error
import           Pos.Ssc.Functions
import           Pos.Ssc.Logic
import           Pos.Ssc.Mem
import           Pos.Ssc.Message
import           Pos.Ssc.SecretStorage
import           Pos.Ssc.Seed
import           Pos.Ssc.State
import           Pos.Ssc.Toss
import           Pos.Ssc.Types
import           Pos.Ssc.VssCertData
