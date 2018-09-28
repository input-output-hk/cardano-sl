{-# LANGUAGE LambdaCase #-}
module Statistics.Graph
    ( graphF
    , writeGraph
    ) where

import           Universum hiding (unlines)

import           Control.Foldl (Fold (..))
import           Data.Graph.Inductive.Graph (Graph (mkGraph))
import           Data.Graph.Inductive.PatriciaTree (Gr)
import           Data.GraphViz (DotGraph)
import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as A
import           Data.GraphViz.Commands.IO (hPutDot)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Pos.Util (withTempFile)
import           Prelude (unlines)
import           System.Exit (ExitCode (ExitSuccess))
import           System.IO (hGetContents, hPutStrLn)
import           System.Process (readProcessWithExitCode)

import           JSONLog (IndexedJLTimedEvent)
import           Statistics.Block (BlockHeader (..), blockChain, blockHeadersF)
import           Types

graphF :: Fold IndexedJLTimedEvent (DotGraph Int)
graphF = f <$> blockHeadersF
  where
    f :: Map BlockHash BlockHeader -> DotGraph Int
    f m =
        let nodes = zip [1..] $ [bh | (_, bh) <- M.toList m]
            h2i   = M.fromList [(bhHash bh, i) | (i, bh) <- nodes]
            edges = do
                (i, bh) <- nodes
                let h = bhHash bh
                    h' = bhPrevBlock $ m M.! h
                guard $ M.member h' h2i
                return (i, h2i M.! h', ())
            g     = mkGraph nodes edges :: Gr BlockHeader ()
        in  G.graphToDot G.nonClusteredParams { G.fmtNode = fmt } g
      where
        chain :: Set BlockHash
        chain = blockChain m

        fmt :: (Int, BlockHeader) -> G.Attributes
        fmt (_, bh) = [ A.Label $ A.StrLabel $ toLText $ unlines labels
                      , A.FillColor (if S.member (bhHash bh) chain
                            then [A.WC (A.X11Color G.Yellow) Nothing]
                            else [A.WC (A.X11Color G.LightGray) Nothing])
                      , A.Style [A.SItem A.Filled []]
                      ]
          where
            labels :: [String]
            labels = [ '#' : take 6 (toString $ bhHash bh)
                     , show (bhNode bh)
                     , show (bhSlot bh)
                     ]

writeGraph :: FilePath -> DotGraph Int -> IO Bool
writeGraph f g = withTempFile "." "graph.dot" $ \_ h -> do
    hPutDot h g
    b <- G.isGraphvizInstalled
    case b of
        True -> do
            input <- hGetContents h
            ex <- view _1 <$> readProcessWithExitCode "dot" ["-Tpng", "-o" ++ f] input
            case ex of
                ExitSuccess -> return True
                _           -> hPutStrLn Universum.stderr ("Creating the graph failed, " ++ show ex) >> return False
        False -> hPutStrLn stderr "Cannot produce graph without dot. Please install graphviz." >> return False
