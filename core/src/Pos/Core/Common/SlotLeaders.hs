module Pos.Core.Common.SlotLeaders
       ( SlotLeaders
       , slotLeadersF
       ) where

import           Universum

import           Formatting (Format, bprint, later)
import           Serokell.Util (enumerate, listChunkedJson, pairBuilder)

import           Pos.Core.Common.StakeholderId

-- | 'NonEmpty' list of slot leaders.
type SlotLeaders = NonEmpty StakeholderId

-- | Pretty-printer for slot leaders. Note: it takes list (not
-- 'NonEmpty' as an argument, because one can always convert @NonEmpty
-- a@ to @[a]@, but it also may be convenient to use it with a simple
-- list of slot leaders).
--
-- Example:
-- [
--    (0, 44283ce5), (1, 5f53e01e), (2, 44283ce5), (3, 1a1ff703), (4, 44283ce5), (5, 44283ce5), (6, 281e5ae9), (7, 1a1ff703)
--    (8, 1a1ff703), (9, 5f53e01e), (10, 1a1ff703), (11, 44283ce5), (12, 44283ce5), (13, 5f53e01e), (14, 5f53e01e), (15, 5f53e01e)
--    (16, 44283ce5), (17, 281e5ae9), (18, 281e5ae9), (19, 44283ce5)
-- ]
slotLeadersF :: Format r ([StakeholderId] -> r)
slotLeadersF =
    later $ bprint (listChunkedJson 8) . map pairBuilder . enumerate @Int
