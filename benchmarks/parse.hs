#!/usr/bin/env stack
-- stack script --resolver lts-9.17 --package conduit-combinators --package conduit-extra --package containers --package bytestring --package unix
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

{- running GHCi
    stack ghci --package conduit-combinators --package text --package conduit-extra --package containers --package bytestring --package unix

    :set -XOverloadedStrings
-}

module ParseBlocks
where

import           Conduit
import           Control.Monad (forM)
import           Prelude hiding (isPrefixOf, putStrLn)
import qualified Data.Conduit.Binary as CB
import           Data.ByteString.Char8 hiding (head)
import qualified Data.List as L (length, map, sortBy)
import           Data.Map as M
import           System.Posix.Env.ByteString (getArgs)

type BlockRel = M.Map ByteString ByteString
type BlockPair = (ByteString, ByteString)

interpretBP :: ByteString -> BlockPair
interpretBP s = read s'
  where s' = unpack s

list2map :: [ByteString] -> BlockRel
list2map ls = M.fromList $ L.map ( interpretBP ) ls

successors :: Int -> BlockRel -> ByteString -> IO (Int)
successors n br h = do
  putStrLn $ ((pack . show) n) `append` " : " `append` h
  case M.lookup h br of
    Just suc -> successors (n + 1) br suc
    Nothing  -> return (n)

findBlock rb h =
  case M.toList $ M.filter (\v -> h `isPrefixOf` v) rb of
    [] -> ""
    ll -> (snd . head) ll


{- entry point -}
main :: IO ()
main = do
  args <- getArgs
  if L.length args < 1
     then do
        error "need at least one hash (prefix) as argument"

     else do
        {- read list of pairs from stdin -}
        ll <- runConduitRes $ stdinC
          .| CB.lines
          .| sinkList

        {- create map of hashes -}
        rb <- return $ list2map ll

        {- for every argument on the command line -}
        forks <- forM args (\h0 -> do
                                     -- find a block hash that starts with <h0>
                                     h <- return $ findBlock rb h0
                                     putStrLn h
                                     -- find successors to a block hash
                                     n <- successors 0 rb h
                                     return $ (n, h)
                           )
        case L.sortBy (\(v1,v2) (w1,w2) -> (-v1) `compare` (-w1)) forks of
          []        -> print "[]"
          ((n,h):_) -> putStrLn $ ((pack . show) n) `append` " " `append` h
  return ()

