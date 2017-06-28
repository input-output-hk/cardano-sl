{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QuasiQuotes   #-}

module KeygenOptions
       ( KeygenOptions (..)
       , AvvmStakeOptions (..)
       , TestStakeOptions (..)
       , FakeAvvmOptions (..)
       , getKeygenOptions
       ) where

import           Data.String.QQ               (s)
import           Data.Version                 (showVersion)
import           Options.Applicative          (Parser, auto, execParser, footerDoc,
                                               fullDesc, header, help, helper, info,
                                               infoOption, long, metavar, option,
                                               progDesc, short, strOption, switch, value)
import           Text.PrettyPrint.ANSI.Leijen (Doc)
import           Universum                    hiding (show)

import           Pos.Core.Types               (StakeholderId)

import           Paths_cardano_sl             (version)

data KeygenOptions = KeygenOptions
    { koRearrangeMask  :: Maybe FilePath
    , koDumpDevGenKeys :: Maybe FilePath
    , koGenesisDir     :: FilePath
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

optionsParser :: Parser KeygenOptions
optionsParser = do
    koRearrangeMask <- optional $ strOption $
        long    "rearrange-mask" <>
        metavar "PATTERN" <>
        help    "Secret keyfiles to rearrange."

    koDumpDevGenKeys <- optional $ strOption $
        long    "dump-dev-genesis-keys" <>
        metavar "PATTERN" <>
        help    "Dump keys from genesisDevSecretKeys to files \
                \named according to this pattern."

    koGenesisDir <- strOption $
        long    "genesis-dir" <>
        metavar "DIR" <>
        value   "." <>
        help    "Directory to dump genesis data into"
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
                \(`{}` is a place for number)."
    tsoPoors <- option auto $
        long    "testnet-keys" <>
        short   'n' <>
        metavar "INT" <>
        help    "Number of testnet stakeholders to generate."
    tsoRichmen <- option auto $
        long    "richmen" <>
        short   'm' <>
        metavar "INT" <>
        help    "Number of rich stakeholders to generate."
    tsoRichmenShare <- option auto $
        long    "richmen-share" <>
        metavar "FLOAT" <>
        help    "Percent of stake dedicated to richmen (between 0 and 1)."
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
        help    "JSON file with AVVM stakes data."
    asoIsRandcerts <- switch $
        long    "randcerts" <>
        help    "Whether to include random VSS certificates to genesis data."
    asoHolderKeyfile <- optional $ strOption $
        long    "fileholder" <>
        metavar "FILE" <>
        help    "A keyfile from which to read public key of stakeholder \
                \to which AVVM stakes are delegated."
    asoBlacklisted <- optional $ strOption $
        long    "blacklisted" <>
        metavar "FILE" <>
        help    "Path to the file containing blacklisted addresses \
                \(an address per line)."
    pure AvvmStakeOptions{..}
  where


fakeAvvmParser :: Parser FakeAvvmOptions
fakeAvvmParser = do
    faoSeedPattern <- strOption $
        long    "fake-avvm-seed-pattern" <>
        metavar "PATTERN" <>
        help    "Filename pattern for generated AVVM seeds \
                \(`{}` is a place for number)."
    faoCount <- option auto $
        long    "fake-avvm-entries" <>
        metavar "INT" <>
        help    "Number of fake AVVM stakeholders."
    faoOneStake <- option auto $
        long    "fake-avvm-stake" <>
        metavar "INT" <>
        value   15000000 <>
        help    "A stake assigned to each of fake AVVM stakeholders."
    return FakeAvvmOptions{..}

getKeygenOptions :: IO KeygenOptions
getKeygenOptions = execParser programInfo
  where
    programInfo = info (helper <*> versionOption <*> optionsParser) $
        fullDesc <> progDesc "Produce 'genesis-*' directory with generated keys."
                 <> header "Tool to generate keyfiles."
                 <> footerDoc usageExample

    versionOption = infoOption
        ("cardano-keygen-" <> showVersion version)
        (long "version" <> help "Show version.")

usageExample :: Maybe Doc
usageExample = Just [s|
Command example:

  stack exec -- cardano-keygen                          \
    --genesis-dir genesis                               \
    -f secrets/secret-{}.key                            \
    -m 5                                                \
    -n 1000                                             \
    --richmen-share 0.94                                \
    --testnet-stake 19072918462000000                   \
    --utxo-file /tmp/avvm-files/utxo-dump-last-new.json \
    --randcerts                                         \
    --blacklisted /tmp/avvm-files/full_blacklist.js     \
    --fake-avvm-seed-pattern avvm/fake-{}.seed          \
    --fake-avvm-entries 100

Subdirectory 'genesis-*/nodes' contains keys for uploading to nodes (in cluster).
Subdirectory 'genesis-*/avvm' contains AVVM seeds.
Subdirectory 'genesis-*/secrets' contains secret keys.|]
