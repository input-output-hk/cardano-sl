{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes  #-}
module Options
       ( CLIOptions (..)
       , getOptions
       , PrintMode(..)
       , UOM(..)
       ) where

import           Universum

import qualified NeatInterpolation as N
import           Options.Applicative (Parser, eitherReader, execParser, flag, flag', footerDoc,
                                      fullDesc, header, help, helper, info, long, metavar, option,
                                      progDesc, short, strOption, switch)
import           Text.PrettyPrint.ANSI.Leijen (Doc)

import qualified Pos.Client.CLI as CLI


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
         | Adaptive


data CLIOptions = CLIOptions
    { dbPath      :: !FilePath
    -- ^ Path to the DB to analyse.
    , uom         :: UOM
    , printMode   :: PrintMode
    , incremental :: !Bool
    -- ^ Wether or not render the report
    -- incrementally (i.e. one row at time).
    , commonArgs  :: !CLI.CommonArgs
    }

optionsParser :: Parser CLIOptions
optionsParser = CLIOptions <$> parseDbPath
                           <*> parseUOM
                           <*> (fromMaybe AsciiTable <$> parsePrintMode)
                           <*> parseIncremental
                           <*> CLI.commonArgsParser

parseDbPath :: Parser FilePath
parseDbPath = strOption (long "db" <> metavar "FILEPATH"
                                   <> help    "Location of the database where the blockchain is stored."
                        )

parsePrintMode :: Parser (Maybe PrintMode)
parsePrintMode = optional (option (eitherReader readPrintModeE)
                                  (long "print-mode" <> metavar "[human|csv|table]"
                                                     <> help "Select the desidered rendering mode, one between 'human', 'csv' or 'table'."
                                  )
                          )
  where
    readPrintModeE :: String -> Either String PrintMode
    readPrintModeE "human" = Right Human
    readPrintModeE "table" = Right AsciiTable
    readPrintModeE "csv"   = Right CSV
    readPrintModeE _       = Right AsciiTable -- A sensible default, for now.

parseUOM :: Parser UOM
parseUOM = (parseKB <|> parseMB <|> parseGB <|> parseAdaptive)
        -- If the parser above fails, default to bytes (even if not passed from the CLI).
        <|> parseBytes

parseBytes :: Parser UOM
parseBytes = flag Bytes Bytes (short 'b' <> help "Display block counts in bytes (B).")

parseKB :: Parser UOM
parseKB = flag' KB (short 'k' <> help "Display block counts in kilobytes (KB).")

parseMB :: Parser UOM
parseMB = flag' MB (short 'm' <> help "Display block counts in megabytes (MB).")

parseGB :: Parser UOM
parseGB = flag' GB (short 'g' <> help "Display block counts in gigabytes (GB).")

parseAdaptive :: Parser UOM
parseAdaptive = flag' Adaptive (short 'a' <> help "Display block counts using an adaptive multiplier.")

parseIncremental :: Parser Bool
parseIncremental = switch (short 'i' <> long "incremental" <> help incrementalHelp)

getOptions :: IO CLIOptions
getOptions = execParser programInfo
  where
    programInfo = info (helper <*> optionsParser) $
        fullDesc <> progDesc "Analyze a blockchain and spit out useful metrics."
                 <> header "Cardano SL blockchain generator"
                 <> footerDoc usageExample

usageExample :: Maybe Doc
usageExample = (Just . fromString @Doc . toString @Text) [N.text|
Command example:

  cardano-blockchain-analyser --db /path/to/existing/db
|]

incrementalHelp :: String
incrementalHelp = toString [N.text| Run in incremental mode. In this mode, table output will
be disabled and rendered as a .csv, as is not possible to generate nice-looking tables
whilst reading the blockchain one block at time. You almost always want to be using this
mode for huge blockchains, as it's much more memory efficient.
|]
