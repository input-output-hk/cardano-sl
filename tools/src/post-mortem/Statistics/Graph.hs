{-# LANGUAGE LambdaCase #-}
module Statistics.Graph
    ( graphF
    , writeGraph
    ) where

import           Control.Foldl                     (Fold (..))
import           Data.GraphViz                     (DotGraph)
import qualified Data.GraphViz                     as G
import qualified Data.GraphViz.Attributes.Complete as A
import           Data.GraphViz.Commands.IO         (hPutDot)
import           Data.Graph.Inductive.Graph        (Graph (mkGraph))
import           Data.Graph.Inductive.PatriciaTree (Gr)
import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict                   as M
import           Data.Set                          (Set)
import qualified Data.Set                          as S
import qualified Data.Text                         as T
import qualified Data.Text.Lazy                    as L
import           System.IO                         (hPutStrLn)
import           Turtle                            hiding (FilePath, f, g, toText, stderr)
import qualified Turtle.Prelude                    as T

import           JSONLog                           (IndexedJLTimedEvent)
import           Prelude                           (unlines)
import           Statistics.Block                  (BlockHeader (..), blockHeadersF, blockChain)
import           Types
import           Universum                         hiding (unlines)

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
        fmt (_, bh) = [ A.Label $ A.StrLabel $ L.pack $ unlines labels
                      , A.FillColor (if S.member (bhHash bh) chain
                            then [A.WC (A.X11Color G.Yellow) Nothing]
                            else [A.WC (A.X11Color G.LightGray) Nothing])
                      , A.Style [A.SItem A.Filled []]
                      ]
          where
            labels :: [String]
            labels = [ '#' : take 6 (T.unpack $ bhHash bh)
                     , show (bhNode bh)
                     , show (bhSlot bh)
                     ]

writeGraph :: FilePath -> DotGraph Int -> IO Bool
writeGraph f g = with (T.mktempfile "." "graph.dot") $ \tmp -> do
    with (T.writeonly tmp) $ flip hPutDot g
    with (T.readonly tmp) $ \h -> do
        b <-G.isGraphvizInstalled
        if b 
            then do
                ex <- T.proc "dot" ["-Tpng", toText $ "-o" ++ f] (T.inhandle h)
                case ex of
                    ExitSuccess -> return True
                    _           -> hPutStrLn Universum.stderr ("Creating the graph failed, " ++ show ex) >> return False
            else hPutStrLn stderr "Cannot produce graph without dot. Please install graphviz." >> return False
