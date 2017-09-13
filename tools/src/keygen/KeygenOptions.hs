{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QuasiQuotes   #-}

module KeygenOptions
       ( KeygenOptions (..)
       , KeygenCommand (..)
       , DumpAvvmSeedsOptions (..)
       , GenesisGenOptions (..)
       , AvvmBalanceOptions (..)
       , TestnetBalanceOptions (..)
       , FakeAvvmOptions (..)
       , GenKeysOptions (..)
       , getKeygenOptions
       ) where

import           Universum

import           Data.Version        (showVersion)
import           Options.Applicative (Parser, auto, command, execParser, fullDesc, header,
                                      help, helper, info, infoOption, long, metavar,
                                      option, progDesc, short, strOption, subparser,
                                      value)

import           Pos.Client.CLI      (configurationOptionsParser)
import           Pos.Core            (StakeholderId)
import           Pos.Genesis         (FakeAvvmOptions (..), TestnetBalanceOptions (..))
import           Pos.Launcher        (ConfigurationOptions)

import           Paths_cardano_sl    (version)

data KeygenOptions = KeygenOptions
    { koCommand              :: KeygenCommand
    , koConfigurationOptions :: ConfigurationOptions
    } deriving (Show)

data KeygenCommand
    = RearrangeMask FilePath
    | GenerateKey FilePath
    | ReadKey FilePath
    | DumpAvvmSeeds DumpAvvmSeedsOptions
    | GenerateKeysBySpec GenKeysOptions
    deriving (Show)

data DumpAvvmSeedsOptions = DumpAvvmSeedsOptions
    { dasNumber :: Int
      -- ^ Number of seeds to generate.
    , dasPath   :: FilePath
      -- ^ Path to directory to generate seeds in.
    } deriving (Show)

data GenesisGenOptions = GenesisGenOptions
    { ggoGenesisDir       :: FilePath
      -- ^ Output directory everything will be put into
    , ggoTestBalance      :: Maybe TestnetBalanceOptions
    , ggoAvvmBalance      :: Maybe AvvmBalanceOptions
    , ggoFakeAvvmBalance  :: Maybe FakeAvvmOptions
    , ggoBootStakeholders :: [(StakeholderId, Word16)]
      -- ^ Explicit bootstrap era stakeholders, list of addresses with
      -- weights (@[(A, 5), (B, 2), (C, 3)]@). Setting this
      -- overrides default settings for boot stakeholders (e.g. rich
      -- in testnet stakes).
    , ggoSeed             :: Maybe Integer
      -- ^ Seed to use (when no seed is provided, a secure random generator
      -- is used)
    } deriving (Show)

data GenKeysOptions = GenKeysOptions
    { gkoGenesisJSON :: FilePath
    , gkoOutDir      :: FilePath
    , gkoKeyPattern  :: FilePath
    } deriving (Show)

data AvvmBalanceOptions = AvvmBalanceOptions
    { asoJsonPath      :: FilePath
    , asoHolderKeyfile :: Maybe FilePath
    , asoBlacklisted   :: Maybe FilePath
    } deriving (Show)

keygenCommandParser :: Parser KeygenCommand
keygenCommandParser =
    subparser $ mconcat $
    [ command "rearrange"
      (infoH rearrangeMask (progDesc "Rearrange keyfiles."))
    , command "generate-key"
      (infoH generateKey (progDesc "Generate keyfile."))
    , command "read-key"
      (infoH readKey (progDesc "Dump keyfile contents."))
    , command "generate-avvm-seeds"
      (infoH (fmap DumpAvvmSeeds dumpAvvmSeedsParser)
            (progDesc "Generate avvm seeds with public keys."))
    , command "generate-keys-by-spec"
      (infoH (GenerateKeysBySpec <$> keysBySpecParser)
            (progDesc "Generate secret keys and avvm seed by genesis-spec.yaml"))
    ]
  where
    infoH a b = info (helper <*> a) b
    rearrangeMask = fmap RearrangeMask . strOption $
        long    "mask" <>
        metavar "PATTERN" <>
        help    "Secret keyfiles to rearrange."
    generateKey = fmap GenerateKey . strOption $
        long    "path" <>
        metavar "PATH" <>
        help    "Write the key to this path"
    readKey = fmap ReadKey . strOption $
        long "path" <>
        metavar "PATH" <>
        help "Dump the contents of this keyfile"

dumpAvvmSeedsParser :: Parser DumpAvvmSeedsOptions
dumpAvvmSeedsParser = do
    dasNumber <-
        option auto $
        long "count" <> short 'n' <> metavar "INTEGER" <>
        help "Number of seeds to generate."
    dasPath <-
        strOption $
        long "output" <> short 'o' <> metavar "FILEPATH" <>
        help "Path to dump generated seeds to."
    pure $ DumpAvvmSeedsOptions {..}

keysBySpecParser  :: Parser GenKeysOptions
keysBySpecParser = do
    gkoGenesisJSON <- strOption $
        long    "genesis-spec" <>
        metavar "FILE" <>
        value   "genesis-spec.yaml" <>
        help    "Genesis file (.yaml)."
    gkoOutDir <- strOption $
        long    "genesis-out-dir" <>
        metavar "DIR" <>
        value   "." <>
        help    "Directory to dump keys and avvm seeds into."
    gkoKeyPattern <- strOption $
        long    "file-pattern" <>
        metavar "PATTERN" <>
        value   "key{}.sk" <>
        help    "Filename pattern for generated keyfiles \
                \(`{}` is a place for number). E.g. key{}.sk"
    pure $ GenKeysOptions {..}

getKeygenOptions :: IO KeygenOptions
getKeygenOptions = execParser programInfo
  where
    programInfo = info (helper <*> versionOption <*> koParser) $
        fullDesc <> header "Tool to generate keyfiles-related data"

    versionOption = infoOption
        ("cardano-keygen-" <> showVersion version)
        (long "version" <> help "Show version.")

    koParser = KeygenOptions <$> keygenCommandParser <*> configurationOptionsParser
