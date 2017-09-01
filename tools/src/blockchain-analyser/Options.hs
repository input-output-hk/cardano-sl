{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QuasiQuotes   #-}
module Options
       ( CLIOptions (..)
       , getOptions
       ) where

import           Universum

import           NeatInterpolation            (text)
import           Options.Applicative          (Parser, execParser, footerDoc, fullDesc,
                                               header, help, helper, info, long, metavar,
                                               progDesc, strOption, value)
import           Text.PrettyPrint.ANSI.Leijen (Doc)

data CLIOptions = CLIOptions
    { dbPath :: !FilePath
    -- ^ Path to the DB to analyse.
    }

optionsParser :: Parser CLIOptions
optionsParser = do
    dbPath <- strOption $
        long    "db" <>
        metavar "FILEPATH" <>
        value   "db-path" <>
        help    "Location of the database where the blockchain is stored."

    return CLIOptions{..}

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

  stack exec -- cardano-block-gen           \
    --blocks 5000                           \
    --nodes 3                               \
    --generated-db /path/to/existed/db      \
    --seed 123
    --append|]
