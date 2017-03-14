{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Transaction related functions.

module Pos.Txp.Core.Tx
       ( topsortTxs
       ) where

import           Control.Lens        (makeLenses, to, (%=), (.=))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import           Data.List           (tail)
import           Universum

import           Pos.Crypto          (Hash, WithHash (..))
import           Pos.Txp.Core.Types  (Tx (..), TxIn (..), txInputs)

----------------------------------------------------------------------------
-- Topsorting
----------------------------------------------------------------------------

data TopsortState a = TopsortState
    { _tsVisited     :: HS.HashSet (Hash Tx)
    , _tsUnprocessed :: [a]
    , _tsResult      :: [a]
    , _tsLoop        :: Bool
    }

$(makeLenses ''TopsortState)

-- | Does topological sort on things that contain transactions – e.g. can be
-- used both for sorting @[Tx]@ and @[(Tx, TxWitness)]@.
--
-- (Backwards dfs from every node with reverse visiting order
-- recording. Returns nothing on loop encountered. Return order is
-- head-first.)
topsortTxs :: forall a. (a -> WithHash Tx) -> [a] -> Maybe [a]
topsortTxs toTx input =
    let res = execState dfs1 initState
    in guard (not $ res ^. tsLoop) >> pure (reverse $ res ^. tsResult)
  where
    dup a = (a,a)
    txHashes :: HashMap (Hash Tx) a
    txHashes = HM.fromList $ map (over _1 (whHash . toTx) . dup) input
    initState = TopsortState HS.empty input [] False
    -- Searches next unprocessed vertix and calls dfs2 for it. Wipes
    -- visited vertices.
    dfs1 :: State (TopsortState a) ()
    dfs1 = unlessM (use tsLoop) $ do
        t <- head <$> use tsUnprocessed
        whenJust t $ \a -> do
            let tx = toTx a
            ifM (HS.member (whHash tx) <$> use tsVisited)
                (tsUnprocessed %= tail)
                (dfs2 HS.empty a tx)
            dfs1
    -- Does dfs putting vertices into tsResult in reversed order of
    -- visiting. visitedThis is map of visited vertices for _this_ dfs
    -- (cycle detection).
    dfs2 :: HashSet (Hash Tx) -> a -> WithHash Tx -> State (TopsortState a) ()
    dfs2 visitedThis _ (WithHash _ txHash)
        | txHash `HS.member` visitedThis = tsLoop .= True
    dfs2 visitedThis a (WithHash tx txHash) = unlessM (use tsLoop) $ do
        tsVisited %= HS.insert txHash
        let visitedNew = HS.insert txHash visitedThis
            dependsUnfiltered =
                mapMaybe (\x -> HM.lookup (txInHash x) txHashes) (tx ^. txInputs . to toList)
        depends <- filterM
            (\x -> not . HS.member (whHash (toTx x)) <$> use tsVisited)
            dependsUnfiltered
        forM_ depends $ \a' -> dfs2 visitedNew a' (toTx a')
        tsResult %= (a:)
