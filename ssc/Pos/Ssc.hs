-- | Re-exports of SSC modules.
--
-- We implement SSC as a coin tossing protocol with guaranteed output
-- delivery. Nodes exchange commitments, openings, and shares, and in the
-- end arrive at a shared seed.
--
-- See https://eprint.iacr.org/2016/889.pdf (“A Provably Secure
-- Proof-of-Stake Blockchain Protocol”), section 4 for more details.

{-# OPTIONS_GHC -F -pgmF autoexporter #-}

import           Pos.Binary.Ssc ()
