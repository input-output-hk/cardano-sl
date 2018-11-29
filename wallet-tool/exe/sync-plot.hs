{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase #-}

module Main where

import           Universum hiding ((.~))
import           Options.Applicative
import Data.Aeson hiding ((.=))
import Data.Vector ((!))
import qualified Data.Vector as V
import Data.List (isSuffixOf)
import Data.Char (toLower)

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
  records <- loadRecords poInputFile
  plotRecords poPlot poBoth poOutputFile records

data PlotType = PlotSync | PlotRestore deriving (Show, Eq)

data PlotOptions = PlotOptions
  { poPlot :: PlotType
  , poBoth :: Bool
  , poInputFile :: FilePath
  , poOutputFile :: FilePath
  } deriving (Show, Eq)

optionsParser :: Parser PlotOptions
optionsParser = PlotOptions <$> plotP <*> leftP <*> inputP <*> outputP
  where
    plotP = subparser
      ( command "sync" (info (pure PlotSync) (progDesc "Blockchain sync chart"))
        <> command "restore" (info (pure PlotRestore) (progDesc "Wallet restore chart"))
        <> commandGroup "Plot type"
      )
    leftP = flag True False (long "left" <> help "Only plot first dataset")
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

plotRecords :: PlotType -> Bool -> FilePath -> [Record] -> IO ()
plotRecords pl both outfile rs = toFile (def & fo_format .~ formatFromFileName outfile) outfile $ do
  layoutlr_title .= plotTitle pl
  layoutlr_left_axis . laxis_override .= axisGridHide
  layoutlr_right_axis . laxis_override .= axisGridHide
  layoutlr_x_axis . laxis_title .= ("Time (seconds since started at " <>  :: String)

  case pl of
    PlotSync -> do
      plotLeft (line "Local chain height" [ [ (t,h) | Record t [h,_] <- rs] ])
      when both $ do
        plotLeft (line "Remote chain height" [ [ (t,h) | Record t [_,h] <- rs] ])
      layoutlr_left_axis . laxis_title .= "Blocks"
    PlotRestore -> do
      plotLeft (line "Restore completion" [ [ (t,pc) | Record t [pc,_] <- rs] ])
      layoutlr_left_axis . laxis_title .= "Percent"
      when both $ do
        plotRight (line "Rate" [ [ (t,r) | Record t [_,r] <- rs] ])
        layoutlr_right_axis . laxis_title .= "Blocks/second"

plotTitle :: PlotType -> String
plotTitle PlotSync = "Blockchain sync"
plotTitle PlotRestore = "Wallet restore"

loadRecords :: FilePath -> IO [Record]
loadRecords = eitherDecodeFileStrict >=> handle
  where
    handle (Left err) = die err >> pure []
    handle (Right rs) = pure (unDataFile rs)
