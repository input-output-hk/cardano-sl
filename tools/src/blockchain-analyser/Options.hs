{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes  #-}
module Options
       ( CLIOptions (..)
       , getOptions
       , PrintMode(..)
       , UOM(..)
       ) where

import           Universum

import qualified NeatInterpolation            as N
import           Options.Applicative          (Parser, eitherReader, execParser, flag,
                                               flag', footerDoc, fullDesc, header, help,
                                               helper, info, long, metavar, option,
                                               progDesc, short, strOption)
import           Text.PrettyPrint.ANSI.Leijen (Doc)


data PrintMode =
      Human
    -- ^ Render using `Buildable` instances.
    | AsciiTable
    -- ^ Render the data as an ASCII Table.
    | CSV
    -- ^ Render the data in CSV format.


-- Unit of measure in use
data UOM = Bytes
         | KB
         | MB
         | GB


data CLIOptions = CLIOptions
    { dbPath    :: !FilePath
    -- ^ Path to the DB to analyse.
    , uom       :: UOM
    , printMode :: PrintMode
    }

optionsParser :: Parser CLIOptions
optionsParser = CLIOptions <$> parseDbPath <*> parseUOM <*> (fromMaybe AsciiTable <$> parsePrintMode)

parseDbPath :: Parser FilePath
parseDbPath = strOption (long "db" <> metavar "FILEPATH"
                                   <> help    "Location of the database where the blockchain is stored."
                        )

parsePrintMode :: Parser (Maybe PrintMode)
parsePrintMode = optional (option (eitherReader readPrintModeE)
                                  (long "print-mode" <> metavar "PRINT-MODE" <> help "Select the desidered rendering mode.")
                          )
  where
    readPrintModeE :: String -> Either String PrintMode
    readPrintModeE "human" = Right Human
    readPrintModeE "table" = Right AsciiTable
    readPrintModeE "csv"   = Right CSV
    readPrintModeE _       = Right AsciiTable -- A sensible default, for now.

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
