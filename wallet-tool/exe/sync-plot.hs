{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase #-}

module Main where

import           Universum hiding ((.~))
import           Options.Applicative
import Data.Aeson hiding ((.=))
import Data.Vector ((!))
import qualified Data.Vector as V
import Data.List (isSuffixOf)
import Data.Char (toLower)
import qualified Data.Text as T

import Graphics.Rendering.Chart.Easy hiding (argument, both)
import Graphics.Rendering.Chart.Backend.Cairo

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
        <> progDesc "Plot the results of a wallet sync" )

run :: PlotOptions -> IO ()
run PlotOptions{..} = do
  df <- loadRecords poInputFile
  plotRecords poPlot poExtraTitle poPlotLeft poPlotRight poOutputFile df

data PlotType = PlotSync | PlotRestore deriving (Show, Eq)

data PlotOptions = PlotOptions
  { poPlot :: PlotType
  , poExtraTitle :: Text
  , poPlotLeft :: Bool
  , poPlotRight :: Bool
  , poInputFile :: FilePath
  , poOutputFile :: FilePath
  } deriving (Show, Eq)

optionsParser :: Parser PlotOptions
optionsParser = PlotOptions <$> plotP <*> titleP <*> leftP <*> rightP <*> inputP <*> outputP
  where
    plotP = subparser
      ( command "sync" (info (pure PlotSync) (progDesc "Blockchain sync chart"))
        <> command "restore" (info (pure PlotRestore) (progDesc "Wallet restore chart"))
        <> commandGroup "Plot type"
      )
    leftP = flag True False (long "no-left" <> help "Don't plot first dataset")
    rightP = flag True False (long "no-right" <> help "Don't plot second dataset")
    titleP = strOption (long "extra-title" <> short 't' <> value "" <> help "Append text to chart title")
    inputP = argument str (metavar "INFILE" <> help "Input JSON")
    outputP = argument str (metavar "OUTFILE" <> help "Output PNG")

data Record = Record Double [Double] deriving (Show)
data DataFile = DataFile
  { dfRecords :: [Record]
  , dfStartTime :: Text
  }

instance FromJSON DataFile where
  parseJSON = withObject "DataFile" $ \ob -> DataFile <$> ob .: "data" <*> ob .:? "start_time" .!= ""

instance FromJSON Record where
  parseJSON = withArray "Record" $ \v ->
    if V.length v == 2
      then (Record <$> parseJSON (v ! 0) <*> parseJSON (v ! 1))
      else fail "Incorrect record array length"

formatFromFileName :: FilePath -> FileFormat
formatFromFileName f | ".png" `isSuffixOf` map toLower f = PNG
                     | otherwise = SVG

timeAxisLabel :: Text -> String
timeAxisLabel startTime = ("Time (seconds" <> extra <> ")")
  where extra | T.null startTime = ""
              | otherwise = " since started at " <> T.unpack startTime

plotRecords :: PlotType -> Text -> Bool -> Bool -> FilePath -> DataFile -> IO ()
plotRecords pl title pleft pright outfile (DataFile rs startTime) =
  toFile (def & fo_format .~ formatFromFileName outfile) outfile $ do
    layoutlr_title .= plotTitle pl <> T.unpack title
    layoutlr_left_axis . laxis_override .= axisGridHide
    layoutlr_right_axis . laxis_override .= axisGridHide
    layoutlr_x_axis . laxis_title .= timeAxisLabel startTime

    let showRight = pright && pl == PlotRestore
    layoutlr_right_axis_visibility . axis_show_line .= showRight
    layoutlr_right_axis_visibility . axis_show_ticks .= showRight
    layoutlr_right_axis_visibility . axis_show_labels .= showRight

    case pl of
      PlotSync -> do
        when pleft $ do
          plotLeft (line "Local block height" [ [ (t,h) | Record t [h,_] <- rs] ])
        when pright $ do
          plotLeft (line "Remote block height" [ [ (t,h) | Record t [_,h] <- rs] ])
        layoutlr_left_axis . laxis_title .= "Blocks"
      PlotRestore -> do
        when pleft $ do
          plotLeft (line "Restore completion" [ [ (t,pc) | Record t [pc,_] <- rs] ])
          layoutlr_left_axis . laxis_title .= "Percent"
        when pright $ do
          plotRight (line "Rate" [ [ (t,r) | Record t [_,r] <- rs] ])
          layoutlr_right_axis . laxis_title .= "Blocks/second"

plotTitle :: PlotType -> String
plotTitle PlotSync = "Blockchain sync"
plotTitle PlotRestore = "Wallet restore"

loadRecords :: FilePath -> IO DataFile
loadRecords = eitherDecodeFileStrict >=> handle
  where
    handle (Left err) = die err >> pure (DataFile [] "")
    handle (Right rs) = pure rs
