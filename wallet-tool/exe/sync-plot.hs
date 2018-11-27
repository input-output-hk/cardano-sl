{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase #-}

module Main where

import           Universum
import           Options.Applicative
import Data.Aeson hiding ((.=))
import Data.Vector ((!))
import qualified Data.Vector as V

import Graphics.Rendering.Chart.Easy hiding (argument)
import Graphics.Rendering.Chart.Backend.Cairo

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
        <> progDesc "PlotType the results of a wallet sync" )

run :: PlotOptions -> IO ()
run PlotOptions{..} = do
  records <- loadRecords poInputFile
  plotRecords poPlot poOutputFile records

data PlotType = PlotSync | PlotRestore deriving (Show, Eq)

data PlotOptions = PlotOptions
  { poPlot :: PlotType
  , poInputFile :: FilePath
  , poOutputFile :: FilePath
  } deriving (Show, Eq)

optionsParser :: Parser PlotOptions
optionsParser = PlotOptions <$> plotP <*> inputP <*> outputP
  where
    plotP = subparser
      ( command "sync" (info (pure PlotSync) (progDesc "Blockchain sync chart"))
        <> command "restore" (info (pure PlotRestore) (progDesc "Wallet restore chart")))
    inputP = argument str (metavar "INFILE" <> help "Input JSON")
    outputP = argument str (metavar "OUTFILE" <> help "Output JSON")

data Record = Record Double [Double] deriving (Show)
newtype DataFile = DataFile { unDataFile :: [Record] }

instance FromJSON DataFile where
  parseJSON = withObject "DataFile" $ \ob -> DataFile <$> ob .: "data"

instance FromJSON Record where
  parseJSON = withArray "Record" $ \v ->
    if V.length v == 2
      then (Record <$> parseJSON (v ! 0) <*> parseJSON (v ! 1))
      else fail "Incorrect record array length"

plotRecords :: PlotType -> FilePath -> [Record] -> IO ()
plotRecords pl outfile rs = toFile def outfile $ do
  layoutlr_title .= plotTitle pl
  layoutlr_left_axis . laxis_override .= axisGridHide
  layoutlr_right_axis . laxis_override .= axisGridHide
  layoutlr_x_axis . laxis_title .= ("Time (seconds)" :: String)

  case pl of
    PlotSync -> do
      plotLeft (line "Local chain height" [ [ (t,h) | Record t [h,_] <- rs] ])
      plotLeft (line "Remote chain height" [ [ (t,h) | Record t [_,h] <- rs] ])
      layoutlr_left_axis . laxis_title .= "Blocks"
    PlotRestore -> do
      plotLeft (line "Restore completion" [ [ (t,pc) | Record t [pc,_] <- rs] ])
      plotRight (line "Rate" [ [ (t,r) | Record t [_,r] <- rs] ])
      layoutlr_left_axis . laxis_title .= "Percent"
      layoutlr_right_axis . laxis_title .= "Blocks/second"

plotTitle :: PlotType -> String
plotTitle PlotSync = "Blockchain sync"
plotTitle PlotRestore = "Wallet restore"

loadRecords :: FilePath -> IO [Record]
loadRecords = eitherDecodeFileStrict >=> handle
  where
    handle (Left err) = die err >> pure []
    handle (Right rs) = pure (unDataFile rs)
