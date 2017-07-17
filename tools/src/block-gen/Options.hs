{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Command line options

module Options
       ( BlockGenOptions (..)
       , getBlockGenOptions
       ) where

import           Universum

import           Options.Applicative          (Parser, auto, execParser, footerDoc,
                                               fullDesc, header, help, helper, info,
                                               infoOption, long, metavar, option,
                                               progDesc, short, strOption, value)
import           Text.PrettyPrint.ANSI.Leijen (Doc)

import           Pos.Constants                (blkSecurityParam, genesisSlotDuration)
import           Pos.Core                     (Timestamp (..))

data BlockGenOptions = BlockGenOptions
    { bgoN    :: !Int
    -- ^ Number of blocks to generate.
    , bgoPath :: !FilePath
    -- ^ Location of generated database.
    }

optionsParser :: Parser BlockGenOptions
optionsParser = do
    bgoN <- option auto $
        long    "n" <>
        metavar "INT" <>
        help "Length of blockchain."

    bgoPath <- strOption $
        long    "db-path" <>
        metavar "FILEPATH" <>
        value   "generated-db" <>
        help    "Location of generated database."

    return BlockGenOptions{..}

getBlockGenOptions :: IO BlockGenOptions
getBlockGenOptions = execParser programInfo
  where
    programInfo = info (helper <*> versionOption <*> optionsParser) $
        fullDesc <> progDesc "It generates database of node, corresponding to some correct blockchain"
                 <> header "Cardano SL blockchain generator"
                 <> footerDoc usageExample

    versionOption = infoOption
        "cardano-block-gen-1.0"
        (long "version" <> help "Show version.")

usageExample :: Maybe Doc
usageExample = undefined
-- usageExample = Just [s|
-- Command example:

--   stack exec -- cardano-block-gen           \
--     -n 5000                                 \
--     --db-path generated-db|]
