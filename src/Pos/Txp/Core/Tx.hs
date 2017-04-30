{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Transaction related functions.

module Pos.Txp.Core.Tx
       ( topsortTxs
       ) where

import           Control.Lens        (makeLenses, to, uses, (%=), (.=))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import           Data.List           (nub, tail)
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

{-# ANN topsortTxs ("HLint: ignore Use ordNub" :: Text) #-}
-- | Does topological sort on things that contain transactions – e.g. can be
-- used both for sorting @[Tx]@ and @[(Tx, TxWitness)]@.
--
-- (Backwards dfs from every node with reverse visiting order
-- recording. Returns nothing on loop encountered. Return order is
-- head-first.)
topsortTxs :: forall a. (Eq a) => (a -> WithHash Tx) -> [a] -> Maybe [a]
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
                (dfs2 HS.empty a)
            dfs1
    -- Does dfs putting vertices into tsResult in reversed order of
    -- visiting. visitedThis is map of visited vertices for _this_ dfs
    -- (used for loop detection).
    dfs2 :: HashSet (Hash Tx) -> a -> State (TopsortState a) ()
    dfs2 visitedThis a | whHash (toTx a) `HS.member` visitedThis = tsLoop .= True
    dfs2 visitedThis a = do
        let (WithHash tx txHash) = toTx a
        looped <- use tsLoop
        visited <- uses tsVisited $ HS.member txHash
        when (not looped && not visited) $ do
            tsVisited %= HS.insert txHash
            let visitedNew = HS.insert txHash visitedThis
                dependsUnfiltered =
                    nub $ mapMaybe (\x -> HM.lookup (txInHash x) txHashes)
                                   (tx ^. txInputs . to toList)
            depends <- filterM
                (\x -> not . HS.member (whHash (toTx x)) <$> use tsVisited)
                dependsUnfiltered
            for_ depends $ \a' -> dfs2 visitedNew a'
            tsResult %= (a:)
