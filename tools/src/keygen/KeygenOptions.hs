{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QuasiQuotes   #-}

module KeygenOptions
       ( KeygenOptions (..)
       , KeygenCommand (..)
       , DumpAvvmSeedsOptions (..)
       , GenesisGenOptions (..)
       , AvvmBalanceOptions (..)
       , TestBalanceOptions (..)
       , FakeAvvmOptions (..)
       , getKeygenOptions
       ) where

import           Universum

import           Data.Version           (showVersion)
import           Options.Applicative    (Parser, auto, command, execParser, fullDesc,
                                         header, help, helper, info, infoOption, long,
                                         metavar, option, progDesc, short, strOption,
                                         subparser, value)
import           Serokell.Util.OptParse (fromParsec)
import qualified Text.Parsec            as P
import qualified Text.Parsec.Text       as P

import           Pos.Client.CLI         (stakeholderIdParser)
import           Pos.Core               (StakeholderId)
import           Pos.Core.Genesis.Types (FakeAvvmOptions (..), TestBalanceOptions (..))

import           Paths_cardano_sl       (version)

data KeygenOptions = KeygenOptions
    { koCommand :: KeygenCommand
    } deriving (Show)

data KeygenCommand
    = RearrangeMask FilePath
    | DumpDevGenKeys FilePath
    | GenerateKey FilePath
    | ReadKey FilePath
    | DumpAvvmSeeds DumpAvvmSeedsOptions
    | GenerateGenesis GenesisGenOptions
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
    , ggoTestBalance      :: Maybe TestBalanceOptions
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
    , command "dump-dev-keys"
      (infoH dumpKeys (progDesc "Dump CSL dev-mode keys."))
    , command "generate-avvm-seeds"
      (infoH (fmap DumpAvvmSeeds dumpAvvmSeedsParser)
            (progDesc "Generate avvm seeds with public keys."))
    , command "generate-genesis"
      (infoH (fmap GenerateGenesis genesisGenParser)
            (progDesc "Generate CSL genesis files."))
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
    dumpKeys = fmap DumpDevGenKeys . strOption $
        long    "pattern" <>
        metavar "PATTERN" <>
        help    "Dump keys from genesisDevSecretKeys to files \
                \named according to this pattern."

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

genesisGenParser :: Parser GenesisGenOptions
genesisGenParser = do
    ggoGenesisDir <- strOption $
        long    "genesis-dir" <>
        metavar "DIR" <>
        value   "." <>
        help    "Directory to dump genesis data into."
    ggoTestBalance <- optional testBalanceParser
    ggoAvvmBalance <- optional avvmBalanceParser
    ggoFakeAvvmBalance <- optional fakeAvvmParser
    ggoBootStakeholders <- many bootStakeholderParser
    ggoSeed <-
        optional $ option auto $
        long "seed" <> metavar "INTEGER" <>
        help "Seed to use for randomness"
    pure $ GenesisGenOptions{..}

testBalanceParser :: Parser TestBalanceOptions
testBalanceParser = do
    tsoPattern <- strOption $
        long    "file-pattern" <>
        short   'f' <>
        metavar "PATTERN" <>
        value   "testnet{}.key" <>
        help    "Filename pattern for generated keyfiles \
                \(`{}` is a place for number). E.g. key{}.kek"
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
        help    "Percent of balance dedicated to richmen (between 0 and 1)."
    tsoTotalBalance <- option auto $
        long    "testnet-balance" <>
        metavar "INT" <>
        help    "Total coins in genesis balance, excluding RSCoin ledger."
    pure TestBalanceOptions{..}

avvmBalanceParser :: Parser AvvmBalanceOptions
avvmBalanceParser = do
    asoJsonPath <- strOption $
        long    "utxo-file" <>
        metavar "FILE" <>
        help    "JSON file with AVVM balances data."
    asoHolderKeyfile <- optional $ strOption $
        long    "fileholder" <>
        metavar "FILE" <>
        help    "A keyfile from which to read public key of balanceholder \
                \to which AVVM balances are delegated."
    asoBlacklisted <- optional $ strOption $
        long    "blacklisted" <>
        metavar "FILE" <>
        help    "Path to the file containing blacklisted addresses \
                \(an address per line)."
    pure AvvmBalanceOptions{..}

fakeAvvmParser :: Parser FakeAvvmOptions
fakeAvvmParser = do
    faoCount <- option auto $
        long    "fake-avvm-entries" <>
        metavar "INT" <>
        help    "Number of fake AVVM stakeholders."
    faoOneBalance <- option auto $
        long    "fake-avvm-balance" <>
        metavar "INT" <>
        value   15000000 <>
        help    "A balance assigned to each of fake AVVM balanceholders."
    return FakeAvvmOptions{..}

bootStakeholderParser :: Parser (StakeholderId, Word16)
bootStakeholderParser =
    option (fromParsec pairParser) $
        long "bootstakeholder" <>
        metavar "ADDRESS,INTEGER" <>
        help "Explicit boot stakeholder with his stake weight for the boot era."
  where
    pairParser :: P.Parser (StakeholderId, Word16)
    pairParser = do
        st <- stakeholderIdParser
        void $ P.char ','
        d <- word16
        pure (st,d)

    lexeme :: P.Parser a -> P.Parser a
    lexeme p = P.spaces *> p >>= \x -> P.spaces $> x

    word16 :: P.Parser Word16
    word16 = lexeme $ do
        val <- readMaybe <$> P.many1 P.digit
        maybe (fail $ show val <> " is not a valid word16")
              (pure . fromInteger)
              val

getKeygenOptions :: IO KeygenOptions
getKeygenOptions = execParser programInfo
  where
    programInfo = info (helper <*> versionOption <*> (KeygenOptions <$> keygenCommandParser)) $
        fullDesc <> header "Tool to generate keyfiles-related data"

    versionOption = infoOption
        ("cardano-keygen-" <> showVersion version)
        (long "version" <> help "Show version.")
