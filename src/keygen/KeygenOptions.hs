{-# LANGUAGE ApplicativeDo #-}

module KeygenOptions
       ( KeygenOptions (..)
       , AvvmStakeOptions (..)
       , TestStakeOptions (..)
       , optsInfo
       ) where

import           Options.Applicative (Parser, ParserInfo, auto, fullDesc, help, helper,
                                      info, long, metavar, option, option, progDesc,
                                      short, strOption, switch, value)
import           Universum

data KeygenOptions = KO
    { koGenesisFile :: FilePath
    , koTestStake   :: Maybe TestStakeOptions
    , koAvvmStake   :: Maybe AvvmStakeOptions
    }

data TestStakeOptions = TSO
    { tsoPattern      :: FilePath
    , tsoPoors        :: Word
    , tsoRichmen      :: Word
    , tsoRichmenShare :: Double
    , tsoTotalStake   :: Word64
    }

data AvvmStakeOptions = ASO
    { asoJsonPath      :: FilePath
    , asoIsRandcerts   :: Bool
    , asoHolderKeyfile :: Maybe FilePath
    }

optsParser :: Parser KeygenOptions
optsParser = do
    koGenesisFile <- strOption $
        long    "genesis-file" <>
        metavar "FILE" <>
        value   "genesis.bin" <>
        help    "File to dump binary shared genesis data"
    koTestStake <- optional testStakeParser
    koAvvmStake <- optional avvmStakeParser
    pure KO{..}

testStakeParser :: Parser TestStakeOptions
testStakeParser = do
    tsoPattern <- strOption $
        long    "file-pattern" <>
        short   'f' <>
        metavar "PATTERN" <>
        help    "Filename pattern for generated keyfiles \
                \(`{}` is a place for number)"
    tsoPoors <- option auto $
        long    "testnet-keys" <>
        short   'n' <>
        metavar "INT" <>
        help    "Number of testnet stakeholders to generate"
    tsoRichmen <- option auto $
        long    "richmen" <>
        short   'm' <>
        metavar "INT" <>
        help    "Number of rich stakeholders to generate"
    tsoRichmenShare <- option auto $
        long    "richmen-share" <>
        metavar "FLOAT" <>
        value   0.7 <>
        help    "Percent of stake dedicated to richmen"
    tsoTotalStake <- option auto $
        long    "total-stake" <>
        metavar "INT" <>
        help    "Total coins in genesis stake, excluding RSCoin ledger."
    pure TSO{..}

avvmStakeParser :: Parser AvvmStakeOptions
avvmStakeParser = do
    asoJsonPath <- strOption $
        long    "utxo-file" <>
        metavar "FILE" <>
        help    "JSON file with AVVM stakes data"
    asoIsRandcerts <- switch $
        long    "randcerts" <>
        help    "Whether to include random VSS certificates to genesis data"
    asoHolderKeyfile <- optional $ strOption $
        long    "fileholder" <>
        metavar "FILE" <>
        help    "A keyfile from which to read public key of stakeholder \
                \to which AVVM stakes are delegated."
    pure ASO{..}

optsInfo :: ParserInfo KeygenOptions
optsInfo = info (helper <*> optsParser) $
    fullDesc `mappend` progDesc "Tool to generate keyfiles"
