#!/usr/bin/env stack
-- stack script --resolver lts-9.17 --package bytestring --package conduit-combinators --package conduit-extra --package containers --package optparse-applicative --package unix
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

{- running GHCi
    stack ghci --package bytestring --package conduit-combinators --package conduit-extra --package containers --package optparse-applicative --package text --package unix

    :set -XOverloadedStrings
-}

module ParseBlocks
where

import           Prelude hiding (putStrLn, isPrefixOf)
import           Control.Monad (forM)
import           Conduit
-- import qualified Data.Conduit.Text as CT
import qualified Data.Conduit.Binary as CB
-- import qualified Data.Conduit.List as CL
import qualified Data.List as L (sortBy, length, map)
import           Data.Map as M
import           Data.ByteString.Char8 hiding (head)
import           System.Posix.Env.ByteString (getArgs)

type BlockRel = M.Map ByteString ByteString
type BlockPair = (ByteString, ByteString)

{-
list2map ["(\"aaaa\",\"bbbb\")", "(\"cccc\",\"dddd\")"]
-}

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
    Nothing -> return (n)

findBlock rb h =
  case M.toList $ M.filter (\v -> h `isPrefixOf` v) rb of
    [] -> ""
    ll -> (snd . head) ll


{- entry point -}
main :: IO ()
main = do
    let parser = some (argument str (metavar "Block hashes ..."))
        opts = info parser mempty
    args <- fmap (map pack) (execParser opts :: IO [String])

    {- read list of pairs from stdin -}
    ll <- runConduitRes $ stdinC
        .| CB.lines
        .| sinkList

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
          [] -> print "[]"
          ((n,h):_) -> putStrLn $ ((pack . show) n) `append` " " `append` h
  return ()

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
