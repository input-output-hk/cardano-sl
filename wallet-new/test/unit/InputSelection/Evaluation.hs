{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE TemplateHaskell #-}

module InputSelection.Evaluation (
    Resolution(..)
  , PlotParams(..)
  , defaultPlotParams
  , evaluateInputPolicies
  ) where

import           Universum

import           Control.Exception (throwIO)
import           Control.Lens ((%=), (+=), (.=), (<<+=))
import           Control.Lens.TH (makeLenses)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Conduit
import           Data.Fixed (E2, Fixed)
import qualified Data.Text.IO as Text
import           Formatting (build, sformat, (%))
import qualified Prelude
import           System.Directory (createDirectory)
import           System.FilePath ((<.>), (</>))
import qualified System.IO.Error as IO
import           Text.Printf (printf)

import           InputSelection.Generator (Event (..), World (..))
import qualified InputSelection.Generator as Gen
import           InputSelection.Policy (InputSelectionPolicy, PrivacyMode (..), RunPolicy (..),
                                        TxStats (..))
import qualified InputSelection.Policy as Policy
import           Util.Distr
import           Util.Histogram (Bin, BinSize (..), Count, Histogram)
import qualified Util.Histogram as Histogram
import qualified Util.MultiSet as MultiSet
import           Util.Range (Range (..), Ranges (..), SplitRanges (..))
import qualified Util.Range as Range
import           Util.StrictStateT
import           UTxO.DSL

{-------------------------------------------------------------------------------
  Plot parameters
-------------------------------------------------------------------------------}

-- | Resolution of the resulting file
data Resolution = Resolution {
      resolutionWidth  :: Int
    , resolutionHeight :: Int
    }

data PlotParams = PlotParams {
      -- | Prefix (path) for all generated files
      prefix      :: FilePath

      -- | Binsize for the UTxO histogram
    , utxoBinSize :: BinSize

      -- | Resolution of the resulting images
      -- This should have a 2:1 aspect ratio.
    , resolution  :: Resolution
    }

defaultPlotParams :: FilePath -> PlotParams
defaultPlotParams prefix = PlotParams {
      prefix      = prefix
    , utxoBinSize = BinSize 10
    , resolution  = Resolution {
                        resolutionWidth  = 800
                      , resolutionHeight = 400
                      }
    }

{-------------------------------------------------------------------------------
  Statistics about the current value of the system
-------------------------------------------------------------------------------}

-- | Statistics about the /current/ value of the system
--
-- This information is solely based on the current value of the system, and not
-- affected by the system history. Consequently this does /NOT/ have a 'Monoid'
-- instance.
data CurrentStats = CurrentStats {
      -- | Current UTxO size
      currentUtxoSize      :: !Int

      -- | Current UTxO histogram
    , currentUtxoHistogram :: !Histogram
    }

deriveCurrentStats :: BinSize -> Utxo h a -> CurrentStats
deriveCurrentStats binSize utxo = CurrentStats {
      currentUtxoSize      = utxoSize utxo
    , currentUtxoHistogram = utxoHistogram binSize utxo
    }

utxoHistogram :: BinSize -> Utxo h a -> Histogram
utxoHistogram binSize =
    Histogram.discretize binSize . map fromIntegral . outputs
  where
    outputs :: Utxo h a -> [Value]
    outputs = map (outVal . snd) . utxoToList

{-------------------------------------------------------------------------------
  Auxiliary: time series
-------------------------------------------------------------------------------}

-- | Time series of values
--
-- When we render a single frame,
--
-- * for some vars we render the /current/ value;
--   for example, we render the current UTxO histogram
-- * for some vars we render the /summarized/ value;
--   for example, we render the overall histogram of number of inputs / tx
-- * for some vars we render a /time series/, from the start until this frame
--   for example, we show the UTxO growth over time
--
-- The 'TimeSeries' is meant to record the third kind of variable.
--
-- NOTE: This is isomorphic to @NewestFirst []@ but is strict.
data TimeSeries a = StartOfTime | MostRecent !a !(TimeSeries a)
  deriving (Functor)

timeSeriesNewestFirst :: TimeSeries a -> [a]
timeSeriesNewestFirst StartOfTime       = []
timeSeriesNewestFirst (MostRecent x xs) = x : timeSeriesNewestFirst xs

-- | Bounds for a time series
timeSeriesRange :: forall a. (Num a, Ord a)
                => TimeSeries a -> Ranges Int a
timeSeriesRange = go . timeSeriesNewestFirst
  where
    go :: [a] -> Ranges Int a
    go xs = Ranges {
          _x = Range 0 (length  xs)
        , _y = Range 0 (maximum xs)
        }

-- | Write out a time series to disk
writeTimeSeries :: forall a. Show a
                => FilePath       -- ^ File to write to
                -> (Int -> Bool)  -- ^ Steps to render
                -> TimeSeries a   -- ^ Time series to render
                -> IO ()
writeTimeSeries fp shouldRender =
       go
     . filter (shouldRender . fst)
     . zip [0..]
     . reverse
     . timeSeriesNewestFirst
  where
    -- We go through LBS to take advantage of it's chunking policy, avoiding
    -- hPutStr and co's excessive lock taking and releasing.
    go :: [(Int, a)] -> IO ()
    go = LBS.writeFile fp
       . LBS.pack
       . Prelude.unlines
       . map (\(step, a) -> Prelude.show step ++ "\t" ++ Prelude.show a)

{-------------------------------------------------------------------------------
  Accumulated statistics
-------------------------------------------------------------------------------}

-- | Accumulated statistics at a given step
data AccStats = AccStats {
      -- | Step counter
      --
      -- This is just a simple counter, incremented after each 'Event'
      _accStep             :: !Int

      -- | Number of payment requests we failed to satisfy
    , _accFailedPayments   :: !Int

      -- | Size of the UTxO over time
    , _accUtxoSize         :: !(TimeSeries Int)

      -- | Maximum UTxO histogram
      --
      -- While not particularly meaningful as statistic to display, this is
      -- useful to determine bounds for rendering.
    , _accUtxoMaxHistogram :: !Histogram

      -- | Transaction statistics
    , _accTxStats          :: !TxStats

      -- | Time series of the median change/payment ratio
    , _accMedianRatio      :: !(TimeSeries (Fixed E2))
    }

makeLenses ''AccStats

initAccumulatedStats :: AccStats
initAccumulatedStats = AccStats {
      _accStep             = 0
    , _accFailedPayments   = 0
    , _accUtxoSize         = StartOfTime
    , _accUtxoMaxHistogram = Histogram.empty
    , _accTxStats          = mempty
    , _accMedianRatio      = StartOfTime
    }

-- | Construct statistics for the next step
--
-- For the median ratio timeseries, we use a default value of @-1@ as long as
-- there are no outputs generated yet (since we plot from 0, this will then be
-- not visible).
stepAccStats :: CurrentStats -> AccStats -> AccStats
stepAccStats CurrentStats{..} st =
    st & accStep              %~ succ
       & accUtxoMaxHistogram  %~ Histogram.max currentUtxoHistogram
       & accUtxoSize          %~ MostRecent currentUtxoSize
       & accMedianRatio       %~ MostRecent (MultiSet.medianWithDefault (-1) txStatsRatios)
  where
    TxStats{..} = st ^. accTxStats

{-------------------------------------------------------------------------------
  Interpreter state
-------------------------------------------------------------------------------}

data IntState h a = IntState {
      _stUtxo       :: !(Utxo h a)
    , _stPending    :: !(Utxo h a)
    , _stStats      :: !AccStats
    , _stFreshHash  :: !Int

      -- | Change address
      --
      -- NOTE: At the moment we never modify this; we're not evaluating
      -- privacy, so change to a single address is fine.
    , _stChangeAddr :: !a

      -- | Binsize used for histograms
      --
      -- We cannot actually currently change this as we run the interpreter
      -- because `Histogram.max` only applies to histograms wit equal binsizes.
    , _stBinSize    :: !BinSize
    }

makeLenses ''IntState

initIntState :: PlotParams -> Utxo h a -> a -> IntState h a
initIntState PlotParams{..} utxo changeAddr = IntState {
      _stUtxo       = utxo
    , _stPending    = utxoEmpty
    , _stStats      = initAccumulatedStats
    , _stFreshHash  = 1
    , _stChangeAddr = changeAddr
    , _stBinSize    = utxoBinSize
    }

instance Monad m => RunPolicy (StrictStateT (IntState h a) m) a where
  genChangeAddr = use stChangeAddr
  genFreshHash  = stFreshHash <<+= 1

{-------------------------------------------------------------------------------
  Interpreter proper
-------------------------------------------------------------------------------}

-- | Current UTxO statistics and accumulated statistics for each step
mkFrame :: Monad m => StrictStateT (IntState h a) m (CurrentStats, AccStats)
mkFrame = state aux
  where
    aux :: IntState h a -> ((CurrentStats, AccStats), IntState h a)
    aux st = ((currentStats, accStats), st & stStats .~ accStats)
      where
        !currentStats = deriveCurrentStats (st ^. stBinSize) (st ^. stUtxo)
        !accStats     = stepAccStats currentStats (st ^. stStats)

-- | Interpreter for events, evaluating a policy
--
-- Turns a stream of events into a stream of observations and accumulated
-- statistics.
--
-- Returns the final state
intPolicy :: forall h a m. (Hash h a, Monad m)
          => InputSelectionPolicy h a (StrictStateT (IntState h a) m)
          -> (a -> Bool)
          -> IntState h a -- Initial state
          -> ConduitT (Event h a) (CurrentStats, AccStats) m (IntState h a)
intPolicy policy ours initState =
    execStrictStateC initState $
      awaitForever $ \event -> do
        lift $ go event
        yield =<< lift mkFrame
  where
    go :: Event h a -> StrictStateT (IntState h a) m ()
    go (Deposit new) =
        stUtxo %= utxoUnion new
    go NextSlot = do
        -- TODO: May want to commit only part of the pending transactions
        pending <- use stPending
        stUtxo    %= utxoUnion pending
        stPending .= utxoEmpty
    go (Pay outs) = do
        utxo <- use stUtxo
        mtx  <- policy utxo outs
        case mtx of
          Right (tx, txStats) -> do
            stUtxo               %= utxoRemoveInputs (trIns tx)
            stPending            %= utxoUnion (utxoRestrictToAddr ours (trUtxo tx))
            stStats . accTxStats %= mappend txStats
          Left _err ->
            stStats . accFailedPayments += 1

{-------------------------------------------------------------------------------
  Compute bounds
-------------------------------------------------------------------------------}

-- | Frame bounds
--
-- When we render all frames, they should also use the same bounds for the
-- animation to make any sense. This is also useful to have animations of
-- _different_ policies use the same bounds.
data Bounds = Bounds {
      -- | Range of the UTxO
      _boundsUtxoHistogram :: SplitRanges Bin Count

      -- | Range of the UTxO size time series
    , _boundsUtxoSize      :: Ranges Int Int

      -- | Range of the transaction inputs
    , _boundsTxInputs      :: Ranges Int Int

      -- | Range of the median change/payment time series
    , _boundsMedianRatio   :: Ranges Int Double
    }

makeLenses ''Bounds

-- | Derive compute final bounds from accumulated statistics
--
-- We hardcode some decisions:
--
-- * For the UTxO we set the minimum count to 0, always.
--   (We don't really have a choice since we can only look at the "maximum"
--   UTxO here, for which the minimum Y values aren't very interesting).
-- * The same goes for the number of transaction inputs: here we only have
--   the final counts.
-- * For number of transaciton inputs we set minimum x to 0 always.
-- * We split the X-range of the UTxO for gaps larger than 100k. This separates
--   the "very large UTxO" from everything else.
-- * For the change/payment ratio, we use a fixed yrange [0:2]. Anything below
--   0 doesn't make sense (we use this for absent values); anything above 2
--   isn't particularly interesting.
deriveBounds :: AccStats -> Bounds
deriveBounds AccStats{..} = Bounds {
      _boundsUtxoHistogram = Histogram.splitRanges 100000 _accUtxoMaxHistogram
                           & Range.splitYRange . Range.lo .~ 0
    , _boundsTxInputs      = Histogram.range (txStatsNumInputs _accTxStats)
                           & Range.x . Range.lo .~ 0
                           & Range.y . Range.lo .~ 0
    , _boundsUtxoSize      = timeSeriesRange (fromIntegral <$> _accUtxoSize)
    , _boundsMedianRatio   = timeSeriesRange _accMedianRatio
                           & Range.y .~ Range 0 2
    }

{-------------------------------------------------------------------------------
  Gnuplot output
-------------------------------------------------------------------------------}

-- | Plot instructions
--
-- As we render the observations, we collect a bunch of plot instructions.
-- The reason that we do not execute these as we go is that we do not know
-- a priori which ranges we should use for the graphs (and it is important
-- that we use the same range for all frames).
data PlotInstr = PlotInstr {
      -- | File prefix for current-step data
      --
      -- I.e., this is data like the UTxO and number of transaction inputs
      -- histogram (but not time series data, see 'piStep').
      piFilePrefix :: FilePath

      -- | Frame counter
      --
      -- This is used to determine how much of the time series data
      -- we want to display.
    , piFrame :: Int

      -- | Number of failed payment attempts
    , piFailedPayments :: Int
    }

-- | Render in gnuplot syntax
renderPlotInstr :: BinSize    -- ^ Bin size (for width of the boxes)
                -> Bounds     -- ^ Derived bounds
                -> Text       -- ^ Set up the split X-axis
                -> Text       -- ^ Reset the split X-axis
                -> PlotInstr  -- ^ Plot instruction
                -> Text
renderPlotInstr utxoBinSize
                bounds
                setupSplitAxis
                resetSplitAxis
                PlotInstr{..} = sformat
    ( "# Frame " % build % "\n"
    % "set output '" % build % ".png'\n"
    % "set multiplot\n"

    -- Plot the current UTxO
    % build
    % "set yrange " % build % "\n"
    % "set size 0.7,1\n"
    % "set origin 0,0\n"
    % "set xtics autofreq rotate by -45\n"
    % "set label 1 'failed: " % build % "' at graph 0.95, 0.90 front right\n"
    % "set boxwidth " % build % "\n"
    % "plot '" % build % ".histogram' using 1:2 with boxes\n"
    % "unset label 1\n"
    % build

    -- Plot UTxO size time series
    % "set xrange " % build % "\n"
    % "set yrange " % build % "\n"
    % "set size 0.50,0.4\n"
    % "set origin 0.05,0.55\n"
    % "unset xtics\n"
    % "plot 'growth' using 1:2 every ::0::" % build % " notitle\n"

    -- Plot transaction number of inputs distribution
    % "set xrange " % build % "\n"
    % "set yrange " % build % "\n"
    % "set size 0.25,0.4\n"
    % "set origin 0.65,0.55\n"
    % "set xtics autofreq rotate by -90\n"
    % "set boxwidth 1\n"
    % "plot '" % build % ".txinputs' using 1:2 with boxes fillstyle solid notitle\n"

    -- Plot average change/payment ratio time series
    % "set xrange " % build % "\n"
    % "set yrange " % build % "\n"
    % "set size 0.25,0.4\n"
    % "set origin 0.65,0.15\n"
    % "unset xtics\n"
    % "plot 'ratio' using 1:2 every ::0::" % build % " notitle\n"

    % "unset multiplot\n"
    )

    -- header
    piFrame
    piFilePrefix

    -- current UTxO
    setupSplitAxis
    (bounds ^. boundsUtxoHistogram . Range.splitYRange)
    piFailedPayments
    utxoBinSize
    piFilePrefix
    resetSplitAxis

    -- UTxO time series
    (bounds ^. boundsUtxoSize . Range.x)
    (bounds ^. boundsUtxoSize . Range.y)
    piFrame

    -- number of inputs
    (bounds ^. boundsTxInputs . Range.x)
    (bounds ^. boundsTxInputs . Range.y)
    piFilePrefix

    -- change:payment ratio time series
    (bounds ^. boundsMedianRatio . Range.x)
    (bounds ^. boundsMedianRatio . Range.y)
    piFrame

-- | Render a complete set of plot instructions
writePlotInstrs :: PlotParams -> FilePath -> Bounds -> [PlotInstr] -> IO ()
writePlotInstrs PlotParams{..} script bounds is = do
    putStrLn $ sformat ("Writing '" % build % "'") script
    withFile script WriteMode $ \h -> do
      Text.hPutStrLn h $ sformat
          ( "set grid\n"
          % "set term png size " % build % ", " % build % "\n"
          % build
          )
          width height
          splitPrelude
      forM_ is $ Text.hPutStrLn h
               . renderPlotInstr
                   utxoBinSize
                   bounds
                   setupSplitAxis
                   resetSplitAxis
  where
    Resolution width height = resolution
    (splitPrelude, setupSplitAxis, resetSplitAxis) =
        Range.renderSplitAxis (binSizeToInt utxoBinSize) 25
          (bounds ^. boundsUtxoHistogram . Range.splitXRanges)

{-------------------------------------------------------------------------------
  Render observations
-------------------------------------------------------------------------------}

-- | Sink that writes statistics to disk
writeStats :: forall m. MonadIO m
           => FilePath      -- ^ Prefix for the files to create
           -> (Int -> Bool) -- ^ Which steps should we render?
           -> ConduitT (CurrentStats, AccStats) Void m [PlotInstr]
writeStats prefix shouldRender =
    loop [] 0
  where
    loop :: [PlotInstr]  -- ^ Accumulator
         -> Int          -- ^ Rendered frame counter
         -> ConduitT (CurrentStats, AccStats) Void m [PlotInstr]
    loop acc frame = do
        mObs <- await
        case mObs of
          Nothing ->
            return $ reverse acc
          Just (curStats, accStats) -> do
            if shouldRender (accStats ^. accStep)
              then do instr <- liftIO $ go frame curStats accStats
                      loop (instr : acc) (frame + 1)
              else loop acc frame

    go :: Int -> CurrentStats -> AccStats -> IO PlotInstr
    go frame CurrentStats{..} accStats = do
        Histogram.writeFile (filepath <.> "histogram") currentUtxoHistogram
        Histogram.writeFile (filepath <.> "txinputs") (txStatsNumInputs txStats)
        return PlotInstr {
            piFilePrefix     = filename
          , piFrame          = frame
          , piFailedPayments = accStats ^. accFailedPayments
          }
      where
        filename = printf "%08d" frame
        filepath = prefix </> filename
        txStats  = accStats ^. accTxStats

{-------------------------------------------------------------------------------
  Run evaluation
-------------------------------------------------------------------------------}

-- | Evaluate a policy
--
-- Returns the accumulated statistics and the plot instructions; we return these
-- separately so that we combine bounds of related plots and draw them with the
-- same scales.
evaluatePolicy :: Hash h a
               => FilePath       -- ^ Path to write to
               -> (Int -> Bool)  -- ^ Frames to render
               -> InputSelectionPolicy h a (StrictStateT (IntState h a) IO)
               -> (a -> Bool)    -- ^ Our addresses
               -> IntState h a   -- ^ Initial state
               -> ConduitT () (Event h a) IO ()
               -> IO (AccStats, [PlotInstr])
evaluatePolicy prefix shouldRender policy ours initState generator =
    fmap (first (view stStats)) $
      runConduit $
        generator                       `fuse`
        intPolicy policy ours initState `fuseBoth`
        writeStats prefix shouldRender

type NamedPolicy h =
    ( String
    , InputSelectionPolicy h World (StrictStateT (IntState h World) IO)
    )


-- | Evaluate various input policies given the specified event stream
--
-- We evaluate
--
-- * Largest-first
-- * Random, privacy mode off
-- * Random, privacy mode on
evaluateUsingEvents :: forall h. Hash h World
                    => PlotParams
                    -> FilePath        -- ^ Prefix for this event stream
                    -> Utxo h World    -- ^ Initial UTxO
                    -> [NamedPolicy h] -- ^ Policies to evaluate
                    -> (Int -> Bool)   -- ^ Frames to render
                    -> ConduitT () (Event h World) IO ()  -- ^ Event stream
                    -> IO ()
evaluateUsingEvents plotParams@PlotParams{..}
                    eventsPrefix
                    initUtxo
                    policies
                    shouldRender
                    events =
    forM_ policies $ \(suffix, policy) -> do
      let prefix' = prefix </> (eventsPrefix ++ suffix)
      go prefix' policy `catch` \e ->
        if IO.isAlreadyExistsError e then
          putStrLn $ "Skipping " ++ prefix' ++ " (directory already exists)"
        else
          throwIO e
  where
    go :: FilePath
       -> InputSelectionPolicy h World (StrictStateT (IntState h World) IO)
       -> IO ()
    go prefix' policy = do
        createDirectory prefix'
        (stats, plotInstr) <- evaluatePolicy
          prefix'
          shouldRender
          policy
          (== Us)
          (initIntState plotParams initUtxo Us)
          events
        writeTimeSeries (prefix' </> "growth") shouldRender (stats ^. accUtxoSize)
        writeTimeSeries (prefix' </> "ratio")  shouldRender (stats ^. accMedianRatio)
        writePlotInstrs
          plotParams
          (prefix' </> "mkframes.gnuplot")
          (deriveBounds stats)
          plotInstr

evaluateInputPolicies :: PlotParams -> IO ()
evaluateInputPolicies plotParams@PlotParams{..} = do
    go "1to1"  initUtxo [largest]   (renderEvery (10 *   3)) $ nTo1  1 500
    go "1to1"  initUtxo [randomOff] (renderEvery (10 *   3)) $ nTo1  1 30000
    go "1to1"  initUtxo [randomOn]  (renderEvery (10 *   3)) $ nTo1  1 30000
    go "3to1"  initUtxo [randomOn]  (renderEvery (10 *   5)) $ nTo1  3 30000
    go "10to1" initUtxo [randomOn]  (renderEvery (10 *  12)) $ nTo1 10 30000
  where
    go = evaluateUsingEvents plotParams

    -- Render every n steps
    renderEvery :: Int -> Int -> Bool
    renderEvery n step = step `mod` n == 0

    -- Event stream with a ratio of N:1 deposits:withdrawals
    nTo1 :: Int -> Int -> ConduitT () (Event GivenHash World) IO ()
    nTo1 n m = Gen.fromDistr Gen.FromDistrParams {
          Gen.fromDistrDep    = NormalDistr 1000 100
        , Gen.fromDistrPay    = NormalDistr
                                  (1000 * fromIntegral n)
                                  (100  * fromIntegral n)
        , Gen.fromDistrNumDep = ConstDistr n
        , Gen.fromDistrNumPay = ConstDistr 1
        , Gen.fromDistrCycles = m
        }

    -- Initial UTxO for all these tests
    initUtxo :: Utxo GivenHash World
    initUtxo = utxoSingleton (Input (GivenHash 0) 0) (Output Us 1000000)

    largest, randomOff, randomOn :: Hash h World => NamedPolicy h
    largest   = ("-largest",   Policy.largestFirst)
    randomOff = ("-randomOff", Policy.random PrivacyModeOff)
    randomOn  = ("-randomOn",  Policy.random PrivacyModeOn)




{-

input selection
coin selection

bitcoinj coin selection? ("multiple classes" -- multiple policies)

https://github.com/bitcoin/bitcoin/issues/7664

See ApproximateBestSubset in wallet.cpp.

sweep?



-}


{-
http://murch.one/wp-content/uploads/2016/11/erhardt2016coinselection.pdf
"An Evaluation of Coin Selection Strategies", Master’s Thesis, Mark Erhardt

2.3.4
A transaction output is labeled as dust when its value is similar to the cost of
spending it.Precisely, Bitcoin Core sets the dust limit to a value where spending an
2.3. Transactions 7
output would exceed 1/3 of its value. T

https://youtu.be/vnHQwYxB08Y?t=39m


https://www.youtube.com/watch?v=IKSSWUBqMCM

companies; hundreds of entries in UTxO
individuals: tens of entries

batch payments!
  (Harding, BitcoinTechTalk -- safe up to 80% of transaction fees)

coin selection --
  relatively minor importance
    batching, better representation (SegWit), .. much larger impact
    coin selection only a few percent savings

* FIFO is actually a reasonable strategy (!)
* So is random
    self correcting -- if you have a large amount of small inputs,
    they'll be more likely to be picked!
    (i.e, if 90% of the wallet is small inputs, 90% change of picking them!)

Branch&Bound seems to do exhaustive search (backtracking algorithm) to find
exact matches, coupled with random selection.

A Traceability Analysis of Monero’s Blockchain
https://eprint.iacr.org/2017/338.pdf
-}
