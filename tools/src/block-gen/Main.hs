-- |

module Main where

import           Universum

import           Mockable            (runProduction)

import           Pos.Generator.Block (genBlocks)

import           Context             (bracketTBlockGenMode)
import           Options             (BlockGenOptions (..), getBlockGenOptions)

main :: IO ()
main = do
    bgo@BlockGenOptions{..} <- getBlockGenOptions
    bgenParams <- undefined

    runProduction $ bracketTBlockGenMode $ do
        blocks <- genBlocks bgenParams
        undefined
