{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Command line options

module Options
       ( BlockGenOptions (..)
       , getBlockGenOptions
       ) where

import           Universum

import           Data.Default                 (def)
import           NeatInterpolation            (text)
import           Options.Applicative          (Parser, auto, execParser, footerDoc,
                                               fullDesc, header, help, helper, info,
                                               infoOption, long, metavar, option,
                                               progDesc, strOption, switch, value)
import           Text.PrettyPrint.ANSI.Leijen (Doc)

import           Pos.Client.CLI               (configurationOptionsParser)
import           Pos.Generator.Block          (TxGenParams (..))
import           Pos.Launcher                 (ConfigurationOptions)

data BlockGenOptions = BlockGenOptions
    { bgoBlockN               :: !Word32
    -- ^ Number of blocks to generate.
    , bgoSecrets              :: ![FilePath]
    -- ^ Secret files.
    , bgoPath                 :: !FilePath
    -- ^ Location of generated database.
    , bgoAppend               :: !Bool
    -- ^ Whether to append to existing db.
    , bgoSeed                 :: !(Maybe Int)
    -- ^ Generating seed
    , bgoTxGenParams          :: !TxGenParams
    -- ^ Transaction generator parameters
    , bgoConfigurationOptions :: ConfigurationOptions
    -- ^ Configuration to run with
    }

optionsParser :: Parser BlockGenOptions
optionsParser = do
    bgoBlockN <- option auto $
        long    "blocks" <>
        metavar "INT" <>
        help "Length of blockchain."

    bgoSecrets <- many $ strOption $
        long    "secret" <>
        metavar "FILEPATH" <>
        help    "Path to secret"


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

    bgoTxGenParams <- getTxGenParams <$>
        (optional $ option auto $
            long "tx-count" <>
            metavar "(INT,INT)" <>
            help "Tx count range.") <*>
        (optional $ option auto $
            long "tx-max-outs" <>
            metavar "INT" <>
            help "Max number of outputs in tx")

    bgoConfigurationOptions <- configurationOptionsParser

    return BlockGenOptions{..}
  where
    getTxGenParams mTC mMO =
        TxGenParams (fromMaybe (_tgpTxCountRange def) mTC)
                    (fromMaybe (_tgpMaxOutputs def) mMO)

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
    --generated-db /path/to/existed/db      \
    --seed 123
    --append|]
