module Pos.Core.Ssc.SharesMap
       ( SharesMap
       , InnerSharesMap
       ) where

import           Universum

import           Pos.Binary.Class (AsBinary)
import           Pos.Core.Common (StakeholderId)
import           Pos.Crypto (DecShare)

-- | Each node generates several 'SharedSeed's, breaks every
-- 'SharedSeed' into 'Share's, and sends those encrypted shares to
-- other nodes (for i-th commitment at i-th element of NonEmpty
-- list). Then those shares are decrypted.
type InnerSharesMap = HashMap StakeholderId (NonEmpty (AsBinary DecShare))

-- | In a 'SharesMap', for each node we collect shares which said node
-- has received and decrypted:
--
--   * Outer key = who decrypted the share
--   * Inner key = who created the share
--
-- Let's say that there are participants {A, B, C}. If A has generated a
-- secret and shared it, A's shares will be denoted as Aa, Ab and Ac (sent
-- correspondingly to A itself, B and C). Then node B will decrypt its share
-- and get Ab_dec; same for other nodes and participants. In the end, after
-- the second phase of the protocol completes and we gather everyone's
-- shares, we'll get the following map:
--
-- @
-- { A: {A: Aa_dec, B: Ba_dec, C: Ca_dec}
-- , B: {A: Ab_dec, B: Bb_dec, C: Cb_dec}
-- , C: {A: Ac_dec, B: Bc_dec, C: Cc_dec}
-- }
-- @
--
-- (Here there's only one share per node, but in reality there'll be more.)
type SharesMap = HashMap StakeholderId InnerSharesMap
