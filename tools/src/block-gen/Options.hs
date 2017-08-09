{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Command line options

module Options
       ( BlockGenOptions (..)
       , getBlockGenOptions
       ) where

import           Universum

import           NeatInterpolation            (text)
import           Options.Applicative          (Parser, auto, execParser, footerDoc,
                                               fullDesc, header, help, helper, info,
                                               infoOption, long, metavar, option,
                                               progDesc, strOption, switch, value)
import           Text.PrettyPrint.ANSI.Leijen (Doc)

import           Pos.Core                     (isDevelopment)

data BlockGenOptions = BlockGenOptions
    { bgoBlockN :: !Word32
    -- ^ Number of blocks to generate.
    , bgoNodes  :: !(Either Word32 [FilePath])
    -- ^ Secret files.
    , bgoPath   :: !FilePath
    -- ^ Location of generated database.
    , bgoAppend :: !Bool
    -- ^ Whether to append to existing db.
    , bgoSeed   :: !(Maybe Int)
    }

optionsParser :: Parser BlockGenOptions
optionsParser = do
    bgoBlockN <- option auto $
        long    "blocks" <>
        metavar "INT" <>
        help "Length of blockchain."

    bgoNodes <-
        if isDevelopment then Left <$> nodesCountParser
        else Right <$> secretsParser

    bgoPath <- strOption $
        long    "generated-db" <>
        metavar "FILEPATH" <>
        value   "generated-db" <>
        help    "Location of generated database."

    bgoAppend <- switch $
        long "append" <>
        help "If database already exists, append to it."

    bgoSeed <- optional $ option auto $
        long    "seed" <>
        metavar "INT" <>
        help    "Custom seed to generate blocks."

    return BlockGenOptions{..}
  where
    secretsParser = many $ strOption $
        long    "secret" <>
        metavar "FILEPATH" <>
        help    "Path to secret"

    nodesCountParser = option auto $
        long    "nodes" <>
        metavar "INT" <>
        help "Number of nodes."

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
usageExample = (Just . fromString @Doc . toString @Text) [text|
Command example:

  stack exec -- cardano-block-gen           \
    --blocks 5000                           \
    --nodes 3                               \
    --coins 100
    --generated-db /path/to/existed/db      \
    --seed 123
    --append|]
