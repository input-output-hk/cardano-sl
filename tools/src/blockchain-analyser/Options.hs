{-# LANGUAGE QuasiQuotes #-}
module Options
       ( CLIOptions (..)
       , getOptions
       ) where

import           Universum

import           NeatInterpolation            (text)
import           Options.Applicative          (Parser, execParser, footerDoc, fullDesc,
                                               header, help, helper, info, long, metavar,
                                               progDesc, strOption)
import           Text.PrettyPrint.ANSI.Leijen (Doc)

data CLIOptions = CLIOptions
    { dbPath :: !FilePath
    -- ^ Path to the DB to analyse.
    }

optionsParser :: Parser CLIOptions
optionsParser = CLIOptions <$> parseDbPath

parseDbPath :: Parser FilePath
parseDbPath = strOption (long "db" <> metavar "FILEPATH"
                                   <> help    "Location of the database where the blockchain is stored."
                        )

getOptions :: IO CLIOptions
getOptions = execParser programInfo
  where
    programInfo = info (helper <*> optionsParser) $
        fullDesc <> progDesc "Analyse a blockchain and spit out useful metrics."
                 <> header "Cardano SL blockchain generator"
                 <> footerDoc usageExample

usageExample :: Maybe Doc
usageExample = (Just . fromString @Doc . toString @Text) [text|
Command example:

  cardano-blockchain-analyser --db /path/to/existing/db
|]
