module InputSelection.Evaluation.Options (
    -- * Options
    Resolution(..)
  , EvalOptions(..)
  , SimulationOptions(..)
  , ReplotOptions(..)
  , ReplayOptions(..)
  , Command(..)
    -- * Parser
  , getEvalOptions
  ) where

import           Universum

import           Data.Fixed (E2, Fixed)
import           Options.Applicative

import           Util.Histogram

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

-- | Resolution of the resulting file
data Resolution = Resolution {
      resolutionWidth  :: Int
    , resolutionHeight :: Int
    }

data EvalOptions = EvalOptions {
      -- | Prefix (path) for all generated files
      prefix      :: FilePath

      -- | Binsize for the UTxO histogram
    , utxoBinSize :: BinSize

      -- | Resolution of the resulting images
      -- This should have a 2:1 aspect ratio.
    , resolution  :: Resolution

      -- | Command to run
    , evalCommand :: Command
    }

data SimulationOptions = SimulationOptions {
      -- | Number of cycles (slots)
      numCycles   :: Int

      -- Number of frames we want for each animation
    , numFrames   :: Int

      -- Initial balance
    , initBalance :: Word64
    }

data ReplotOptions = ReplotOptions {
      -- | Splits for the x-axis for the UTxO histogram
      replotSplits    :: [Int]

      -- | Maximum Y for the ratio range
    , replotRatioMaxY :: Maybe (Fixed E2)

      -- | Subdirectory of the results to regenerate
    , replotSubdir    :: String
    }

data ReplayOptions = ReplayOptions {
      -- | Multiplier to apply to the data
      replayMultiplier  :: Double

      -- | Render every @n@ slots
      --
      -- Since we do not a priori know how many slots the file has, we cannot
      -- compute this value from the total number of slots and a desired number
      -- of frames.
    , replayRenderEvery :: Int

      -- | File with data
    , replayFile        :: FilePath
    }

data Command =
      RunSimulation SimulationOptions
    | Replay ReplayOptions
    | Replot ReplotOptions

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

parseResolution :: Parser Resolution
parseResolution = mkResolution
    <$> (option auto $ mconcat [
             long "height"
           , help "Height of the resulting image (width will be twice height)"
           , value 400
           , showDefault
           ])
  where
    mkResolution :: Int -> Resolution
    mkResolution height = Resolution {
          resolutionWidth  = height * 2
        , resolutionHeight = height
        }

parseOptions :: Parser EvalOptions
parseOptions = EvalOptions
    <$> (strOption $ mconcat [
            long "prefix"
          , help "Prefix (path) for all generated files"
          , value "result"
          , showDefault
          ])
    <*> (option (BinSize <$> auto) $ mconcat [
            long "binsize"
          , help "Binsize for histogram discretization"
          , value (BinSize 10)
          , showDefault
          ])
    <*> parseResolution
    <*> parseCommand

parseSimulationOptions :: Parser SimulationOptions
parseSimulationOptions = SimulationOptions
    <$> (option auto $ mconcat [
             long "cycles"
           , help "Number of cycles"
           , value 100000
           , showDefault
           ])
    <*> (option auto $ mconcat [
            long "frames"
          , help "Number of frames to generate"
          , value 200
          , showDefault
          ])
    <*> (option auto $ mconcat [
            long "init-balance"
          , help "Initial balance"
          , value 1000000
          , showDefault
          ])

parseReplotOptions :: Parser ReplotOptions
parseReplotOptions = ReplotOptions
    <$> (many $ option auto $ mconcat [
            long "split"
          , help "Introduce split on the UTxO x-axis (can be used multiple times)"
          ])
    <*> (optional $ option auto $ mconcat [
            long "ratio-max-y"
          , help "Maximum Y value for the change:payment ratio plot"
          ])
    <*> (argument str $ mconcat [
            help "Subdirectory of the results directory"
          , metavar "PATH"
          ])

parseReplayOptions :: Parser ReplayOptions
parseReplayOptions = ReplayOptions
    <$> (option auto $ mconcat [
            help "Multiply all input values"
          , long "multiply"
          , value 1.0
          , showDefault
          ])
    <*> (option auto $ mconcat [
            help "Render a frame every N slots"
          , long "render-every"
          , value 1
          , showDefault
          , metavar "N"
          ])
    <*> (argument str $ mconcat [
            help "File containing values to replay. Should contain one deposit (positive number) or payment (negative number) per line."
          , metavar "PATH"
          ])

parseCommand :: Parser Command
parseCommand = subparser $ mconcat [
      cmd "run" (RunSimulation <$> parseSimulationOptions) "Run the simulation"
    , cmd "replot" (Replot <$> parseReplotOptions) "Reconstruct 'mkframes.gnuplot'"
    , cmd "replay" (Replay <$> parseReplayOptions) "Replay event stream from file"
    ]
  where
    cmd :: String -> Parser Command -> String -> Mod CommandFields Command
    cmd label parser description =
      command label (info (parser <**> helper) $ mconcat [
          progDesc description
        ])

{-------------------------------------------------------------------------------
  Get options
-------------------------------------------------------------------------------}

getEvalOptions :: IO (Maybe EvalOptions)
getEvalOptions = do
    args <- getArgs
    case args of
      ("eval" : args') ->
        Just <$> handleParseResult (execParserPure defaultPrefs opts args')
      _otherwise ->
        return Nothing
  where
    opts :: ParserInfo EvalOptions
    opts = info (parseOptions <**> helper) $ mconcat [
          fullDesc
        , progDesc "Coin selection evaluation"
        ]
