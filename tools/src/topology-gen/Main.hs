{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Main where

import           Control.Monad.State.Strict
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.NodeMap
import           Data.Graph.Inductive.PatriciaTree
import           Data.Graph.Inductive.Query.BFS
import           Data.GraphViz
import           Data.GraphViz.Attributes.Complete
import           Data.List
import           Data.Monoid
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import           System.FilePath ((<.>))
import           Text.PrettyPrint (($$), (<+>))
import qualified Text.PrettyPrint as PP

{- | There are two graph structures used here, one that contains the
   "active" set of nodes/edges (along with the alternatives); the
   second one that contains all possible edges (but with their
   rendering determined by the `PathRole`) - this is to try to ensure
   that the graphviz creates the same node layout for the various
   scenarios (though there may be a better approach).
-}


-- | Graph nodes are annotated with the "name" of the node
type NodeA = (NodeN, NodeState)
type NodeN = String

-- | The nodes state
data NodeState = Alive | Dead deriving (Eq, Ord, Show)

-- | Edges are annotated with the role of that edge and all the
--   alternate destinations for that edge.
type EdgeA = (PathRole, [(PathRole, Node)])

-- | A path can be primary, secondary or inactive (this influences how
--   it is rendered)
data PathRole  = Primary | Secondary | Inactive deriving (Eq, Ord, Show)

type GVParams = GraphvizParams Node String PathRole Char String

-- | An example connectivity graph of size 14
layoutA :: (Gr NodeA EdgeA, [LNode NodeA])
layoutA =
    (\(a,(_,b)) -> (b,a)) $ run empty $ do
      let a'c@[a'c1, a'c2, a'c3]
            = fmap (,Alive) $ ["A(C)/" ++  show n | (n::Int) <- [1,2,3]]
      let a'r@[a'r1, a'r2, a'r3]
            = fmap (,Alive) $ ["A(R)/" ++  show n | (n::Int) <- [1,2,3]]
      let b'c@[b'c1, b'c2]
            = fmap (,Alive) $ ["B(C)/" ++  show n | (n::Int) <- [1,2]]
      let b'r@[b'r1, b'r2]
            = fmap (,Alive) $ ["B(R)/" ++  show n | (n::Int) <- [1,2]]
      let c'c@[c'c1, c'c2]
            = fmap (,Alive) $ ["C(C)/" ++  show n | (n::Int) <- [1,2]]
      let c'r@[c'r1, c'r2]
            = fmap (,Alive) $ ["C(R)/" ++  show n | (n::Int) <- [1,2]]

      nodes <- insMapNodesM $ concat [a'c, b'c, c'c,  a'r, b'r, c'r]

      insRoutes a'c1 [[a'c2, a'c3], [a'r1, a'r2], [b'c1, b'c2]]
      insRoutes a'c2 [[a'c3, a'c1], [a'r2, a'r3], [c'c2, c'c1]]
      insRoutes a'c3 [[a'c1, a'c2], [a'r3, a'r1], [b'r1, c'r1]]
      insRoutes a'r1 [[a'c1, a'c3], [a'c2, a'c3], [a'r2, a'r3], [b'r1, b'r2]]
      insRoutes a'r2 [[a'c1, a'c2], [a'c3, a'c2], [a'r3, a'r1], [c'r1, c'r2]]
      insRoutes a'r3 [[a'c2, a'c1], [a'c3, a'c1], [a'r1, a'r2]]

      insRoutes b'c1 [[b'c2, b'r2], [b'r1, b'r2], [c'c1, c'c2]]
      insRoutes b'c2 [[b'c1, b'r1], [b'r2, b'r1], [a'c2, a'c2]]
      insRoutes b'r1 [[b'c1, b'c2], [b'r2, a'r2], [a'r1, a'r3]]
      insRoutes b'r2 [[b'c2, b'c1], [b'r1, a'r2], [c'r2, c'r1]]

      insRoutes c'c1 [[c'c2, c'r1], [c'r1, c'r2], [a'c1, a'c2]]
      insRoutes c'c2 [[c'c1, c'r1], [c'r2, c'r1], [b'c2, b'c1]]
      insRoutes c'r1 [[c'c1, c'c2], [c'r2, a'r2], [a'r3, a'r2]]
      insRoutes c'r2 [[c'c2, c'c1], [c'r1, a'r2], [b'r2, b'r1]]

      return nodes


-- | Insert the primary edge, recording all the alternatives as
--   secondaries associated with that edge.
insRoutes :: NodeA
          -> [[NodeA]]
          -> NodeMapM NodeA EdgeA Gr ()
insRoutes x ys =
    mapM_ (ins x) ys
  where
    ins a bs = do
      let b = head bs
      bs' <- fmap (map fst) $ mkNodesM (tail bs)
      insMapEdgeM ( a
                  , b
                  , (Primary, [(Secondary, z) | z <- bs'])
                  )

-- | flatten the connectivity graph for the purposes of rendering.
flatten :: Gr NodeA EdgeA -> Gr NodeA PathRole
flatten g =
    mkGraph (labNodes g) (concatMap f $ labEdges g)
  where
    f (a,b,(s,cs)) = (a,b,s) : [(a,x,y) | (y,x) <- cs]

-- | Flatten then render the graph as a "dot" file.
render :: Gr NodeA EdgeA -> EscString -> T.Text
render g title =
    printDotGraph . graphToDot (params title) $ (flatten g)
  where
    params title
      = defaultParams
        { globalAttributes
            = [GraphAttrs [LabelJust JCenter, Label (StrLabel title)]]
        , clusterBy = \(n,(l,s)) -> C (head l) $ N (n,(l,s))
        , isDotCluster = const True
        , clusterID    = Str . T.singleton
        , fmtCluster = \c -> [GraphAttrs [toLabel c ]]
        , fmtNode = \(_,(l,s))
          -> [Label (StrLabel (T.pack l))] ++
             case s of {Alive -> []; Dead -> [Style [SItem Dotted [] ]]}
        , fmtEdge = \(_,_,x)
           -> case x of { Primary   -> []
                        ; Secondary -> [Style [SItem Dotted    [] ]]
                        ; Inactive  -> [Style [SItem Invisible [] ]]
                        }
        }

-- | Mark all secondary paths as inactive
secondaryToInactive :: Gr NodeA EdgeA -> Gr NodeA EdgeA
secondaryToInactive =
    emap f
  where
    g Secondary = Inactive
    g x         = x
    f (a,xs)    = (g a, [(g p,q) | (p,q) <- xs])

-- | Mark the given nodes as "dead" - activate any usable secondary paths
markNodesDead :: [Node] -> Gr NodeA EdgeA -> Gr NodeA EdgeA
markNodesDead ns g =
    mkGraph (map nf $ labNodes g) (map ef $ labEdges g)
  where
    nf x@(n', (l,s))
      | n' `elem` ns = (n', (l, Dead))
      | otherwise = x
    ef (a,b,(c,cs))
      | a `elem` ns = (a,b,(Inactive, map markInactive cs))
      | b `elem` ns = (a,b,(Inactive, activateFirst cs))
      | otherwise   = (a,b,(c,map markInactive cs))
    activateFirst [] = []
    activateFirst ((x,xn):xs)
      | xn `elem` ns   = (Inactive, xn) : activateFirst xs
      | x == Secondary = (x,xn) : map markInactive xs
      | otherwise      = (x,xn) : activateFirst xs
    markInactive z@(x,y)
     | x == Secondary = (Inactive, y)
     | otherwise      = z


-- | Check that graph is still fully connected, ie there are no
--   unexpected unreachable nodes. This is done here in a brute force
--   approach (company graph expert on holiday!) by checking that for
--   all the active nodes (ones left marked `Alive` in the flattened
--   graph) all the `Alive` nodes of the graph are reachable - if not
--   returning that node name.
checkConnected :: Gr NodeA EdgeA -> [String]
checkConnected g'' =
    concatMap checkOk $ labNodes g
  where
    g' = flatten g''
    f  = [(a,b)    | (a,(b,x)) <- labNodes g'
                   , x == Alive]
    h  = [(a,b,()) | (a,b,x)   <- labEdges g'
                   , x == Primary || x == Secondary]
    g :: Gr String ()
    g  = mkGraph f h
    g'size = noNodes g
    checkOk (n,l)
      | length (bfs n g) == g'size = []
      | otherwise                  = [l]


-- renderAsConfig :: Gr NodeA EdgeA -> T.Text
renderAsConfig g =
    PP.render $
    PP.vcat
    [ PP.quotes (PP.text name) PP.<> PP.colon
      $$
      PP.nest 2 (PP.text "static-routes" PP.<> PP.colon <+> routes)
    | (name, routeHops) <- graphToConfig g
    , let routes =
            ppList
              [ ppList (map (PP.doubleQuotes . PP.text) (primary:secondaries))
              | RouteHop primary secondaries <- routeHops ]
    ]
  where
    ppList = PP.brackets . PP.hsep . PP.punctuate PP.comma

data RouteHop n = RouteHop n [n]  -- primary and secondaries
  deriving Show

graphToConfig :: Gr NodeA EdgeA -> [(NodeN, [RouteHop NodeN])]
graphToConfig g =
    [ (nname, routeHops)
    | (nid, (nname, Alive)) <- labNodes g
    , let routeHops = [ RouteHop (label primary) (map label secondaries)
                      | (primary, (Primary, alternates))  <- lsuc g nid
                      , let secondaries = [ n | (Secondary, n) <- alternates ] ]
    ]
  where
    label :: Node -> NodeN
    label n = name where Just (name, Alive) = lab g n

main :: IO ()
main = (flip evalStateT) 0 $ do
    let (the'graph, the'nodes) = layoutA
    outputDot "primary"
      $ render (secondaryToInactive the'graph) "all primary routes"
    outputDot "complete"  $ render the'graph "all routes"
    outputYaml "complete"  $ renderAsConfig the'graph
    mapM_ (withDead the'graph) $ [[a] | a <- the'nodes]
    mapM_ (withDead the'graph)
       $ (nub . sort) [sort [a,b] | a <- the'nodes, b <- the'nodes, a /= b]
    mapM_ (withDead the'graph)
      $ (nub . sort) [sort [a,b,c]
                     | a <- the'nodes
                     , b <- the'nodes
                     , c <- the'nodes
                     , a /= b, a /= c, b /=c ]
  where
    outputYaml :: MonadIO m => FilePath -> String -> m ()
    outputYaml fp = liftIO . writeFile ("config" <.> fp <.> "yaml")
    outputDot :: FilePath -> T.Text -> StateT Int IO ()
    outputDot fp dot = do
      ix <- inc
      liftIO $ T.writeFile ("test" <.> digits 3 ix <.> fp <.> "dot") dot
    inc = get >>= \n -> put (n+1) >> return n
    digits n = reverse . take n . (++ repeat '0') . reverse . show
    withDead :: Gr NodeA EdgeA -> [LNode NodeA] -> StateT Int IO ()
    withDead g ns
      = let g' = markNodesDead (map fst ns) g
            t' = T.intercalate "," [T.pack a | (_,(a,_)) <- ns]
            f' = T.unpack . T.map h $ t'
            h '/' = '-'
            h ',' = '.'
            h  x  = x
            msg = case checkConnected g' of
               [] -> Nothing
               x  -> Just x
            in
      do case msg of
           Nothing -> return ()
           Just m -> liftIO . putStrLn $ (T.unpack t') ++ ": "
                     ++ show m ++ " isolated."
         outputDot f' .  render g' $
           ("with " `mappend` t' `mappend` " dead") `mappend`
            case msg of
              Nothing -> T.empty
              Just m  -> " >>ISOLATED>> " `mappend` (T.pack $ show m)
