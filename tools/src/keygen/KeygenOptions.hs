{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QuasiQuotes   #-}

module KeygenOptions
       ( KeygenOptions (..)
       , KeygenCommand (..)
       , DumpAvvmSeedsOptions (..)
       , GenesisGenOptions (..)
       , AvvmStakeOptions (..)
       , TestStakeOptions (..)
       , FakeAvvmOptions (..)
       , getKeygenOptions
       ) where

import           Data.String.QQ               (s)
import           Data.Version                 (showVersion)
import           Options.Applicative          (Parser, auto, command, execParser,
                                               footerDoc, fullDesc, header, help, helper,
                                               info, infoOption, long, metavar, option,
                                               progDesc, short, strOption, subparser,
                                               value)
import           Serokell.Util.OptParse       (fromParsec)
import qualified Text.Parsec                  as P
import qualified Text.Parsec.String           as P
import           Text.PrettyPrint.ANSI.Leijen (Doc)
import           Universum

import           Pos.Core.Types               (Address (..), StakeholderId)
import           Pos.Types                    (decodeTextAddress)

import           Paths_cardano_sl             (version)

data KeygenOptions = KeygenOptions
    { koCommand :: KeygenCommand
    } deriving (Show)

data KeygenCommand
    = RearrangeMask FilePath
    | DumpDevGenKeys FilePath
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
    , ggoTestStake        :: Maybe TestStakeOptions
    , ggoAvvmStake        :: Maybe AvvmStakeOptions
    , ggoFakeAvvmStake    :: Maybe FakeAvvmOptions
    , ggoBootStakeholders :: [(StakeholderId, Word16)]
      -- ^ Explicit bootstrap era stakeholders, list of addresses with
      -- weights (@[(A, 5), (B, 2), (C, 3)]@). Setting this
      -- overrides default settings for boot stakeholders (e.g. rich
      -- in testnet stakes).
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
    , asoHolderKeyfile :: Maybe FilePath
    , asoBlacklisted   :: Maybe FilePath
    } deriving (Show)

data FakeAvvmOptions = FakeAvvmOptions
    { faoCount    :: Word
    , faoOneStake :: Word64
    } deriving (Show)

keygenCommandParser :: Parser KeygenCommand
keygenCommandParser =
    subparser $ mconcat $
    [ command "rearrange"
      (infoH rearrangeMask (progDesc "Rearrange keyfiles"))
    , command "dump-dev-keys"
      (infoH dumpKeys (progDesc "Dump CSL dev-mode keys"))
    , command "generate-avvm-seeds"
      (infoH (fmap DumpAvvmSeeds dumpAvvmSeedsParser)
            (progDesc "Generate avvm seeds with public keys"))
    , command "generate-genesis"
      (infoH (fmap GenerateGenesis genesisGenParser)
            (progDesc "Generate CSL genesis files"))
    ]
  where
    infoH a b = info (helper <*> a) b
    rearrangeMask = fmap RearrangeMask . strOption $
        long    "mask" <>
        metavar "PATTERN" <>
        help    "Secret keyfiles to rearrange."
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
        help "Number of seeds to generate"
    dasPath <-
        strOption $
        long "output" <> short 'o' <> metavar "FILEPATH" <>
        help "Path to dump generated seeds to"
    pure $ DumpAvvmSeedsOptions {..}

genesisGenParser :: Parser GenesisGenOptions
genesisGenParser = do
    ggoGenesisDir <- strOption $
        long    "genesis-dir" <>
        metavar "DIR" <>
        value   "." <>
        help    "Directory to dump genesis data into"
    ggoTestStake <- optional testStakeParser
    ggoAvvmStake <- optional avvmStakeParser
    ggoFakeAvvmStake <- optional fakeAvvmParser
    ggoBootStakeholders <- many bootStakeholderParser
    pure $ GenesisGenOptions{..}

testStakeParser :: Parser TestStakeOptions
testStakeParser = do
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

fakeAvvmParser :: Parser FakeAvvmOptions
fakeAvvmParser = do
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

bootStakeholderParser :: Parser (StakeholderId, Word16)
bootStakeholderParser =
    option (fromParsec pairParser) $
        long "bootstakeholder" <>
        metavar "ADDRESS,INTEGER" <>
        help "Explicit boot stakeholder with his stake weight for the boot era."
  where
    pairParser :: P.Parser (StakeholderId, Word16)
    pairParser = do
        st <- stakeholderId
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

    stakeholderId :: P.Parser StakeholderId
    stakeholderId = lexeme $ do
        str <- P.many1 P.alphaNum
        case decodeTextAddress (toText str) of
            Left err                  -> fail (toString err)
            Right (PubKeyAddress{..}) -> pure addrKeyHash
            Right p                   ->
                fail $ "Expected public key address, but it's " ++
                       toString (pretty p)

getKeygenOptions :: IO KeygenOptions
getKeygenOptions = execParser programInfo
  where
    programInfo = info (helper <*> versionOption <*> (KeygenOptions <$> keygenCommandParser)) $
        fullDesc <> header "Tool to generate keyfiles-related data"
                 <> footerDoc (Just usageExample)

    versionOption = infoOption
        ("cardano-keygen-" <> showVersion version)
        (long "version" <> help "Show version.")

usageExample :: Doc
usageExample = [s|
Command example:

  stack exec -- cardano-keygen generate-genesis         \
    --genesis-dir genesis                               \
    -m 5                                                \
    -n 1000                                             \
    --richmen-share 0.94                                \
    --testnet-stake 19072918462000000                   \
    --utxo-file /tmp/avvm-files/utxo-dump-last-new.json \
    --randcerts                                         \
    --blacklisted /tmp/avvm-files/full_blacklist.js     \
    --fake-avvm-entries 100                             \
    --bootstakeholder "1fKNcnJ44voGtWmekuKic1HJdbHEwd1YEwZNu6XaAwE8RSk,5" \
    --bootstakeholder "1HJdbHEwd1YEwZNu6XaAwE8RSk1fKNcnJ44voGtWmekuKic,3" \
    --bootstakeholder "8RSk1fKNcnJ41HJdbNu6XaAwE4voGtWmekuHEwd1YEwZKic,2"

Subdirectory 'genesis-*/keys-testnet' contains keys for uploading to nodes (in cluster).
Subdirectory 'genesis-*/keys-fakeavvm' contains AVVM seeds. |]
