{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes  #-}
module Options
       ( CLIOptions (..)
       , getOptions
       , renderBytes
       ) where

import           Universum

import           Formatting                   hiding (bytes)
import qualified NeatInterpolation            as N
import           Options.Applicative          (Parser, execParser, flag, flag', footerDoc,
                                               fullDesc, header, help, helper, info, long,
                                               metavar, progDesc, short, strOption)
import           Text.PrettyPrint.ANSI.Leijen (Doc)

data UOM = Bytes
         | KB
         | MB
         | GB

-- Not using `Serokell.Data.Memory.Units` here, as it will automatically "promote" each
-- unit to the next multiplier. Example:
--
-- > sformat memory (fromBytes @Byte 1000)
-- "1000 B"
-- > sformat memory (fromBytes @Byte 1025)
-- "1.001 KiB"
--
-- What we want, instead, is to always convert from bytes to the requested unit of
-- measure, like other unix tools do.
renderBytes :: UOM -> Integer -> Text
renderBytes uom bytes =
    let (divider, unit) = formatBytes
        formatPrecision = fixed @Double 3
        converted       = fromIntegral bytes / fromIntegral divider
    in sformat (formatPrecision % " " % stext) converted unit
    where
      formatBytes :: (Int, Text)
      formatBytes = case uom of
                        Bytes -> (1, "B")
                        KB    -> (1000, "KB")
                        MB    -> (1000 * 1000, "MB")
                        GB    -> (1000 * 1000 * 1000, "GB")

data CLIOptions = CLIOptions
    { dbPath :: !FilePath
    -- ^ Path to the DB to analyse.
    , uom    :: UOM
    }

optionsParser :: Parser CLIOptions
optionsParser = CLIOptions <$> parseDbPath <*> parseUOM

parseDbPath :: Parser FilePath
parseDbPath = strOption (long "db" <> metavar "FILEPATH"
                                   <> help    "Location of the database where the blockchain is stored."
                        )

parseUOM :: Parser UOM
parseUOM = (parseKB <|> parseMB <|> parseGB)
        -- If the parser above fails, default to bytes (even if not passed from the CLI).
        <|> parseBytes

parseBytes :: Parser UOM
parseBytes = flag Bytes Bytes (short 'b' <> help "Render in bytes.")

parseKB :: Parser UOM
parseKB = flag' KB (short 'k' <> help "Render in kilobytes.")

parseMB :: Parser UOM
parseMB = flag' MB (short 'm' <> help "Render in megabytes.")

parseGB :: Parser UOM
parseGB = flag' GB (short 'g' <> help "Render in gigabytes.")

getOptions :: IO CLIOptions
getOptions = execParser programInfo
  where
    programInfo = info (helper <*> optionsParser) $
        fullDesc <> progDesc "Analyse a blockchain and spit out useful metrics."
                 <> header "Cardano SL blockchain generator"
                 <> footerDoc usageExample

usageExample :: Maybe Doc
usageExample = (Just . fromString @Doc . toString @Text) [N.text|
Command example:

  cardano-blockchain-analyser --db /path/to/existing/db
|]
