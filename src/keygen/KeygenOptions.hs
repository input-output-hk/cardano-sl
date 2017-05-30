{-# LANGUAGE ApplicativeDo #-}

module KeygenOptions
       ( KeygenOptions (..)
       , AvvmStakeOptions (..)
       , TestStakeOptions (..)
       , FakeAvvmOptions (..)
       , optsInfo
       ) where

import           Options.Applicative (Parser, ParserInfo, auto, fullDesc, help, helper,
                                      info, long, metavar, option, progDesc, short,
                                      strOption, switch, value)
import           Universum

data KeygenOptions = KeygenOptions
    { koGenesisDir     :: FilePath
    , koRearrangeMask  :: Maybe FilePath
    , koDumpDevGenKeys :: Maybe FilePath
    , koTestStake      :: Maybe TestStakeOptions
    , koAvvmStake      :: Maybe AvvmStakeOptions
    , koFakeAvvmStake  :: Maybe FakeAvvmOptions
    } deriving (Show)

data TestStakeOptions = TestStakeOptions
    { tsoPattern      :: FilePath
    , tsoPoors        :: Word
    , tsoRichmen      :: Word
    , tsoRichmenShare :: Double
    , tsoTotalStake   :: Word64
    } deriving (Show)

data AvvmStakeOptions = AvvmStakeOptions
    { asoJsonPath      :: FilePath
    , asoIsRandcerts   :: Bool
    , asoHolderKeyfile :: Maybe FilePath
    , asoBlacklisted   :: Maybe FilePath
    } deriving (Show)

data FakeAvvmOptions = FakeAvvmOptions
    { faoSeedPattern :: FilePath
    , faoCount       :: Word
    , faoOneStake    :: Word64
    } deriving (Show)

optsParser :: Parser KeygenOptions
optsParser = do
    koGenesisDir <- strOption $
        long    "genesis-dir" <>
        metavar "DIR" <>
        value   "." <>
        help    "Directory to dump genesis data into"
    koRearrangeMask <- optional $ strOption $
        long    "rearrange-mask" <>
        metavar "PATTERN" <>
        help    "Secret keyfiles to rearrange"
    koDumpDevGenKeys <- optional $ strOption $
        long    "dump-dev-genesis-keys" <>
        metavar "PATTERN" <>
        help    "Dump keys from genesisDevSecretKeys to files \
                \named according to this pattern"
    koTestStake <- optional testStakeParser
    koAvvmStake <- optional avvmStakeParser
    koFakeAvvmStake <- optional fakeAvvmParser
    pure KeygenOptions{..}

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
        help    "Percent of stake dedicated to richmen (between 0 and 1)"
    tsoTotalStake <- option auto $
        long    "testnet-stake" <>
        metavar "INT" <>
        help    "Total coins in genesis stake, excluding RSCoin ledger."
    pure TestStakeOptions{..}

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
    asoBlacklisted <- optional $ strOption $
        long    "blacklisted" <>
        metavar "FILE" <>
        help    "Path to the file containing blacklisted addresses \
                \(an address per line)"
    pure AvvmStakeOptions{..}

fakeAvvmParser :: Parser FakeAvvmOptions
fakeAvvmParser = do
    faoSeedPattern <- strOption $
        long    "fake-avvm-seed-pattern" <>
        metavar "PATTERN" <>
        help    "Filename pattern for generated AVVM seeds \
                \(`{}` is a place for number)"
    faoCount <- option auto $
        long    "fake-avvm-entries" <>
        metavar "INT" <>
        help    "Number of fake avvm stakeholders"
    faoOneStake <- option auto $
        long    "fake-avvm-stake" <>
        metavar "INT" <>
        value   15000000 <>
        help    "A stake assigned to each of fake avvm stakeholders"
    return FakeAvvmOptions{..}

optsInfo :: ParserInfo KeygenOptions
optsInfo = info (helper <*> optsParser) $
    fullDesc `mappend` progDesc "Tool to generate keyfiles"
