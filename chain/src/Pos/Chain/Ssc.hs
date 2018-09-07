-- | Re-exports of SSC modules.
--
-- We implement SSC as a coin tossing protocol with guaranteed output
-- delivery. Nodes exchange commitments, openings, and shares, and in the
-- end arrive at a shared seed.
--
-- See https://eprint.iacr.org/2016/889.pdf (“A Provably Secure
-- Proof-of-Stake Blockchain Protocol”), section 4 for more details.

module Pos.Chain.Ssc
       ( module X
       ) where

import           Pos.Chain.Ssc.Base as X
import           Pos.Chain.Ssc.Behavior as X
import           Pos.Chain.Ssc.Commitment as X
import           Pos.Chain.Ssc.CommitmentAndOpening as X
import           Pos.Chain.Ssc.CommitmentsMap as X
import           Pos.Chain.Ssc.Configuration as X
import           Pos.Chain.Ssc.Error as X
import           Pos.Chain.Ssc.Functions as X
import           Pos.Chain.Ssc.Mem as X
import           Pos.Chain.Ssc.Message as X
import           Pos.Chain.Ssc.Opening as X
import           Pos.Chain.Ssc.OpeningsMap as X
import           Pos.Chain.Ssc.Payload as X
import           Pos.Chain.Ssc.Proof as X
import           Pos.Chain.Ssc.Seed as X
import           Pos.Chain.Ssc.Shares as X (getOurShares)
import           Pos.Chain.Ssc.SharesDistribution as X
import           Pos.Chain.Ssc.SharesMap as X
import           Pos.Chain.Ssc.Toss as X
import           Pos.Chain.Ssc.Types as X
import           Pos.Chain.Ssc.VssCertData as X
import           Pos.Chain.Ssc.VssCertificate as X
import           Pos.Chain.Ssc.VssCertificatesMap as X
