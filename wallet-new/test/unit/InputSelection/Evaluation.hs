{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}

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
import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as Text
import           Data.Time
import           Formatting (build, sformat, (%))
import qualified Prelude
import           Serokell.Util (listJson)
import           System.Directory (createDirectory)
import           System.FilePath ((<.>), (</>))
import           System.IO (hFlush, stdout)
import qualified System.IO.Error as IO
import           Text.Printf (printf)

import           InputSelection.Generator (Event (..), World (..))
import qualified InputSelection.Generator as Gen
import           InputSelection.Policy
import           InputSelection.Policy.LargestFirst
import           InputSelection.Policy.Random
import           Util.Distr
import           Util.Histogram (Bin, BinSize (..), Count, Histogram)
import qualified Util.Histogram as Histogram
import qualified Util.MultiSet as MultiSet
import           Util.Range (Range (..), Ranges (..), SplitRanges (..))
import qualified Util.Range as Range
import           Util.StrictStateT
import           UTxO.DSL (Utxo)
import qualified UTxO.DSL as DSL

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

-- | Statistics about the /current/ value of the system at the end of a slot
--
-- This information is solely based on the current value of the system, and not
-- affected by the system history. Consequently this does /NOT/ have a 'Monoid'
-- instance.
data SlotStats = SlotStats {
      -- | Current UTxO size
      slotUtxoSize      :: !Int

      -- | Current UTxO balance
    , slotUtxoBalance   :: !Value

      -- | Current UTxO histogram
    , slotUtxoHistogram :: !Histogram
    }

deriveSlotStats :: (IsUtxo utxo, Hash h a) => BinSize -> utxo h a -> SlotStats
deriveSlotStats binSize utxo = SlotStats {
      slotUtxoSize      = utxoSize              utxo
    , slotUtxoBalance   = utxoBalance           utxo
    , slotUtxoHistogram = utxoHistogram binSize utxo
    }

utxoHistogram :: (IsUtxo utxo, Hash h a) => BinSize -> utxo h a -> Histogram
utxoHistogram binSize =
    Histogram.discretize binSize . map fromIntegral . utxoOutputs

{-------------------------------------------------------------------------------
  Auxiliary: time series
-------------------------------------------------------------------------------}

type SlotNr = Int

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
-- Since we don't same at each slot, we store this as a spare mapping form
-- slot numbers to values.
newtype TimeSeries a = TimeSeries {
      timeSeriesToMap :: Map SlotNr a
    }
  deriving (Functor)

timeSeriesToList :: TimeSeries a -> [(SlotNr, a)]
timeSeriesToList = Map.toList . timeSeriesToMap

emptyTimeSeries :: TimeSeries a
emptyTimeSeries = TimeSeries Map.empty

insertTimeSeries :: SlotNr -> a -> TimeSeries a -> TimeSeries a
insertTimeSeries slotNr a (TimeSeries m) = TimeSeries (Map.insert slotNr a m)

-- | Bounds for a time series
timeSeriesRange :: forall a. (Num a, Ord a)
                => TimeSeries a -> Ranges SlotNr a
timeSeriesRange (TimeSeries m) = Ranges {
      _x = Range 0 (fst (Map.findMax m))
    , _y = Range 0 (maximum (Map.elems m))
    }

-- | Write out a time series to disk
--
-- Implementation note: go through LBS to take advantage of it's chunking
-- policy, avoiding hPutStr and co's excessive lock taking and releasing.
writeTimeSeries :: forall a. Show a
                => FilePath -> TimeSeries a -> IO ()
writeTimeSeries fp =
       LBS.writeFile fp
     . LBS.pack
     . Prelude.unlines
     . map (\(step, a) -> Prelude.show step ++ "\t" ++ Prelude.show a)
     . timeSeriesToList

{-------------------------------------------------------------------------------
  Accumulated statistics
-------------------------------------------------------------------------------}

-- | Overall statistics that aren't related to slots
data OverallStats = OverallStats {
      -- | Number of payment requests we failed to satisfy
      _overallFailedPayments :: !Int

      -- | Transaction statistics
    , _overallTxStats        :: !TxStats
    }

makeLenses ''OverallStats

initOverallStats :: OverallStats
initOverallStats = OverallStats {
      _overallFailedPayments = 0
    , _overallTxStats        = mempty
    }

-- | Accumulated statistics at the end of each slot
data AccSlotStats = AccSlotStats {
      -- | Maximum UTxO histogram
      --
      -- While not particularly meaningful as statistic to display, this is
      -- useful to determine bounds for rendering.
      _accUtxoMaxHistogram :: !Histogram

      -- | Size of the UTxO over time
    , _accUtxoSize         :: !(TimeSeries Int)

      -- | Total balance of the UTxO over time
    , _accUtxoBalance      :: !(TimeSeries Value)

      -- | Time series of the median change/payment ratio
    , _accMedianRatio      :: !(TimeSeries (Fixed E2))
    }


makeLenses ''AccSlotStats

initAccSlotStats :: AccSlotStats
initAccSlotStats = AccSlotStats {
      _accUtxoMaxHistogram = Histogram.empty
    , _accUtxoSize         = emptyTimeSeries
    , _accUtxoBalance      = emptyTimeSeries
    , _accMedianRatio      = emptyTimeSeries
    }

-- | Construct statistics for the next step
--
-- For the median ratio timeseries, we use a default value of @-1@ as long as
-- there are no outputs generated yet (since we plot from 0, this will then be
-- not visible).
stepAccStats :: SlotNr -> OverallStats -> SlotStats -> AccSlotStats -> AccSlotStats
stepAccStats slotNr OverallStats{..} SlotStats{..} acc =
    acc & accUtxoMaxHistogram %~ Histogram.max slotUtxoHistogram
        & accUtxoSize         %~ insertTimeSeries slotNr slotUtxoSize
        & accUtxoBalance      %~ insertTimeSeries slotNr slotUtxoBalance
        & accMedianRatio      %~ insertTimeSeries slotNr (MultiSet.medianWithDefault (-1) txStatsRatios)
  where
    TxStats{..} = _overallTxStats

{-------------------------------------------------------------------------------
  Interpreter state
-------------------------------------------------------------------------------}

data IntState (utxo :: (* -> *) -> * -> *) h a = IntState {
      -- | Available UTxO
      _stUtxo       :: !(utxo h a)

      -- | Pending UTxO
      --
      -- NOTE: Unlike in the wallet, we don't care about the transactions here.
      -- In the simulation, when we "construct a transaction", its inputs are
      -- immediately removed from '_stUtxo', and the change outputs are added
      -- to '_stPending'. Then at the end of a slot we just add all these
      -- new outputs to 'stUxo'.
    , _stPending    :: !(Utxo h a)

      -- | Running statistics
    , _stStats      :: !OverallStats

      -- | Next hash
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

initIntState :: (IsUtxo utxo, Hash h a)
             => PlotParams -> Utxo h a -> a -> IntState utxo h a
initIntState PlotParams{..} utxo changeAddr = IntState {
      _stUtxo       = fromUtxo utxo
    , _stPending    = utxoEmpty
    , _stStats      = initOverallStats
    , _stFreshHash  = 1
    , _stChangeAddr = changeAddr
    , _stBinSize    = utxoBinSize
    }

instance Monad m => RunPolicy (StrictStateT (IntState utxo h a) m) a where
  genChangeAddr = use stChangeAddr
  genFreshHash  = stFreshHash <<+= 1

{-------------------------------------------------------------------------------
  Composable policies
-------------------------------------------------------------------------------}

data CompPolicy utxo h a m = forall utxo'. IsUtxo utxo' => CompPolicy {
      -- | Construct a transaction
      mkTx :: InputSelectionPolicy utxo h a
                (StrictStateT (IntState utxo h a) m)

      -- | Tell the policy we reached the end of a slot
      --
      -- This gives the policy the change to evolve as well as change
      -- representation, if needed
    , nextSlot :: (utxo h a -> utxo' h a, CompPolicy utxo' h a m)
    }

simpleCompPolicy :: forall utxo h a m. IsUtxo utxo
                 => InputSelectionPolicy utxo h a
                      (StrictStateT (IntState utxo h a) m)
                 -> CompPolicy utxo h a m
simpleCompPolicy p = go
  where
    go :: CompPolicy utxo h a m
    go = CompPolicy p (identity, go)

firstThen :: forall utxo utxo' h a m. (IsUtxo utxo, IsUtxo utxo', Hash h a)
          => InputSelectionPolicy utxo h a
               (StrictStateT (IntState utxo h a) m)
          -> InputSelectionPolicy utxo' h a
               (StrictStateT (IntState utxo' h a) m)
          -> Int
          -> CompPolicy utxo h a m
firstThen p p' = goP
  where
    goP :: Int -> CompPolicy utxo h a m
    goP 0 = CompPolicy p (convertUtxo, simpleCompPolicy p')
    goP n = CompPolicy p (identity, goP (n - 1))

{-------------------------------------------------------------------------------
  Interpreter proper
-------------------------------------------------------------------------------}

-- | Interpreter for events, evaluating a policy
--
-- Turns a stream of events into a stream of observations and accumulated
-- statistics.
intPolicy :: forall utxo h a m. (IsUtxo utxo, Hash h a, Monad m)
          => (a -> Bool)            -- Our addresses
          -> (SlotNr -> Bool)       -- Slots to render
          -> IntState utxo h a      -- Initial state
          -> CompPolicy utxo h a m  -- Initial policy
          -> ConduitT (Event h a)
                      (SlotNr, OverallStats, SlotStats)
                      m
                      OverallStats
intPolicy ours shouldRender =
    loop 0
  where
    deriveSlotStats' :: IsUtxo utxo'
                     => SlotNr
                     -> IntState utxo' h a
                     -> (SlotNr, OverallStats, SlotStats)
    deriveSlotStats' slot st = (
          slot
        , st ^. stStats
        , deriveSlotStats (st ^. stBinSize) (st ^. stUtxo)
        )

    loop :: IsUtxo utxo'
         => SlotNr
         -> IntState utxo' h a
         -> CompPolicy utxo' h a m
         -> ConduitT (Event h a)
                     (SlotNr, OverallStats, SlotStats)
                     m
                     OverallStats
    loop slotNr !st policy@CompPolicy{..} = do
        mEvent <- await
        case mEvent of
          Nothing ->
            return $ st ^. stStats
          Just event -> do
            (isEndOfSlot, st') <- lift $ flip runStrictStateT st $
                                     intEvent mkTx event
            if not isEndOfSlot
              then loop slotNr st' policy
              else do
                when (shouldRender slotNr) $ do
                  yield $ deriveSlotStats' slotNr st'
                let (changeRep, policy') = nextSlot
                loop (slotNr + 1) (st' & stUtxo %~ changeRep) policy'

    intEvent :: IsUtxo utxo'
             => InputSelectionPolicy utxo' h a
                  (StrictStateT (IntState utxo' h a) m)
             -> Event h a
             -> StrictStateT (IntState utxo' h a) m Bool
    intEvent f = \case
      Deposit new -> do
        stUtxo %= utxoUnion new
        return False
      NextSlot -> do
        -- TODO: May want to commit only part of the pending transactions
        pending <- use stPending
        stUtxo    %= utxoUnion pending
        stPending .= utxoEmpty
        return True
      Pay outs -> do
        utxo <- use stUtxo
        mtx  <- f utxo outs
        case mtx of
          Right (tx, txStats, selected) -> do
            stUtxo    %= utxoRemoveInputs selected
            stPending %= utxoUnion (DSL.utxoRestrictToAddr ours (DSL.trUtxo tx))
            stStats . overallTxStats %= mappend txStats
          Left _err ->
            stStats . overallFailedPayments += 1
        return False

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
    , _boundsUtxoSize      :: Ranges SlotNr Int

      -- | Range of the UTxO balance time series
    , _boundsUtxoBalance   :: Ranges SlotNr Value

      -- | Range of the transaction inputs
    , _boundsTxInputs      :: Ranges Int Int

      -- | Range of the median change/payment time series
    , _boundsMedianRatio   :: Ranges SlotNr Double
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
deriveBounds :: OverallStats -> AccSlotStats -> Bounds
deriveBounds OverallStats{..} AccSlotStats{..} = Bounds {
      _boundsUtxoHistogram = Histogram.splitRanges 5 5 _accUtxoMaxHistogram
                           & Range.splitYRange . Range.lo .~ 0
    , _boundsTxInputs      = Histogram.range (txStatsNumInputs _overallTxStats)
                           & Range.x . Range.lo .~ 0
                           & Range.y . Range.lo .~ 0
    , _boundsUtxoSize      = timeSeriesRange (fromIntegral <$> _accUtxoSize)
    , _boundsUtxoBalance   = timeSeriesRange _accUtxoBalance
                           & Range.y . Range.hi %~ (\v -> v + v `div` 100)
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
      piFilePrefix     :: FilePath

      -- | Frame counter
      --
      -- This is used to determine how much of the time series data
      -- we want to display.
    , piFrame          :: Int

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
    % "plot '" % build % ".histogram' using 1:2 notitle with boxes\n"
    % "unset label 1\n"
    % build

    -- Superimpose UTxO size and balance time seriess
    % "set xrange " % build % "\n"
    % "set yrange " % build % "\n"
    % "set y2range " % build % "\n"
    % "set size 0.50,0.4\n"
    % "set origin 0.05,0.55\n"
    % "unset xtics\n"
    % "set y2tics autofreq\n"
    % "plot 'growth'  using 1:2 every ::0::" % build % " notitle axes x1y1 with lines\n"
    % "plot 'balance' using 1:2 every ::0::" % build % " notitle axes x1y2 with lines linecolor rgbcolor 'blue'\n"
    % "unset y2tics\n"

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

    -- Superimposed UTxO size and balance time series
    (bounds ^. boundsUtxoSize . Range.x)
    (bounds ^. boundsUtxoSize . Range.y)
    (bounds ^. boundsUtxoBalance . Range.y)
    piFrame
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
           -> ConduitT (SlotNr, OverallStats, SlotStats) Void m (AccSlotStats, [PlotInstr])
writeStats prefix =
    loop initAccSlotStats [] 0
  where
    loop :: AccSlotStats -- ^ Accumulated slot statistics
         -> [PlotInstr]  -- ^ Accumulated plot instructions
         -> Int          -- ^ Rendered frame counter
         -> ConduitT (SlotNr, OverallStats, SlotStats) Void m (AccSlotStats, [PlotInstr])
    loop !accSlotStats accInstrs frame = do
        mObs <- await
        case mObs of
          Nothing ->
            return (accSlotStats, reverse accInstrs)
          Just (slotNr, overallStats, slotStats) -> do
            instr <- liftIO $ go frame overallStats slotStats
            let accSlotStats' = stepAccStats slotNr overallStats slotStats accSlotStats
                accInstrs'    = instr : accInstrs
                frame'        = frame + 1
            loop accSlotStats' accInstrs' frame'

    go :: Int -> OverallStats -> SlotStats -> IO PlotInstr
    go frame OverallStats{..} SlotStats{..} = do
        Histogram.writeFile (filepath <.> "histogram") slotUtxoHistogram
        Histogram.writeFile (filepath <.> "txinputs") (txStatsNumInputs _overallTxStats)
        return PlotInstr {
            piFilePrefix     = filename
          , piFrame          = frame
          , piFailedPayments = _overallFailedPayments
          }
      where
        filename = printf "%08d" frame
        filepath = prefix </> filename

{-------------------------------------------------------------------------------
  Run evaluation
-------------------------------------------------------------------------------}

-- | Evaluate a policy
--
-- Returns the accumulated statistics and the plot instructions; we return these
-- separately so that we combine bounds of related plots and draw them with the
-- same scales.
evaluatePolicy :: (IsUtxo utxo, Hash h a)
               => FilePath               -- ^ Path to write to
               -> (SlotNr -> Bool)       -- ^ Slots to render
               -> CompPolicy utxo h a IO -- ^ Policy to evaluate
               -> (a -> Bool)            -- ^ Our addresses
               -> IntState utxo h a      -- ^ Initial state
               -> ConduitT () (Event h a) IO ()
               -> IO (OverallStats, (AccSlotStats, [PlotInstr]))
evaluatePolicy prefix shouldRender policy ours initState generator =
      runConduit $
        generator                                    `fuse`
        intPolicy ours shouldRender initState policy `fuseBoth`
        writeStats prefix

data NamedPolicy h = forall utxo. IsUtxo utxo => NamedPolicy {
      namedPolicyName :: String
    , namedPolicy     :: CompPolicy utxo h World IO
    }

-- | Evaluate various input policies given the specified event stream
--
-- We evaluate
--
-- * Largest-first
-- * Random, privacy mode off
-- * Random, privacy mode on
evaluateUsingEvents :: forall h. Hash h World
                    => PlotParams
                    -> FilePath          -- ^ Prefix for this event stream
                    -> Utxo h World      -- ^ Initial UTxO
                    -> [NamedPolicy h]   -- ^ Policies to evaluate
                    -> (SlotNr -> Bool)  -- ^ Slots to render
                    -> ConduitT () (Event h World) IO ()  -- ^ Event stream
                    -> IO ()
evaluateUsingEvents plotParams@PlotParams{..}
                    eventsPrefix
                    initUtxo
                    policies
                    shouldRender
                    events =
    forM_ policies $ \case
      NamedPolicy{..} -> do
        let prefix' = prefix </> (eventsPrefix ++ "-" ++ namedPolicyName)
        go prefix' namedPolicy `catch` \e ->
          if IO.isAlreadyExistsError e then
            putStrLn $ "Skipping " ++ prefix' ++ " (directory already exists)"
          else
            throwIO e
  where
    go :: IsUtxo utxo
       => FilePath
       -> CompPolicy utxo h World IO
       -> IO ()
    go prefix' policy = do
        createDirectory prefix'
        (overallStats, (accStats, plotInstr)) <- evaluatePolicy
          prefix'
          shouldRender
          policy
          (== Us)
          (initIntState plotParams initUtxo Us)
          events
        writeTimeSeries (prefix' </> "growth")  (accStats ^. accUtxoSize)
        writeTimeSeries (prefix' </> "balance") (accStats ^. accUtxoBalance)
        writeTimeSeries (prefix' </> "ratio")   (accStats ^. accMedianRatio)
        writePlotInstrs
          plotParams
          (prefix' </> "mkframes.gnuplot")
          (deriveBounds overallStats accStats)
          plotInstr

evaluateInputPolicies :: PlotParams -> IO ()
evaluateInputPolicies plotParams@PlotParams{..} = do
    go "1to1"  [largest]                   600 $ nTo1  1
    go "1to1"  [randomOff]              100000 $ nTo1  1
    go "1to1"  [randomOn]               100000 $ nTo1  1
    go "3to1"  [largest]                100000 $ nTo1  3
    go "3to1"  [randomOn]               100000 $ nTo1  3
    go "3to1"  [largeThenRandom 100000] 500000 $ nTo1  3
    go "10to1" [randomOn]               100000 $ nTo1 10
  where
    go :: FilePath                -- Prefix for this event stream
       -> [NamedPolicy GivenHash] -- Policies to evaluate
       -> Int                     -- Total number of cycles
       -> (Int -> ConduitT () (Event GivenHash World) IO ())
                    -- Event stream (parameterized by number of cycles)
       -> IO ()
    go eventsPrefix policies numCycles events = do
      putStr $ sformat ("Running " % build % " " % listJson % ".. ")
                 eventsPrefix
                 (map namedPolicyName policies)
      hFlush stdout
      start <- getCurrentTime
      evaluateUsingEvents
        plotParams
        eventsPrefix
        initUtxo
        policies
        (renderEvery (numCycles `div` numFrames))
        (events numCycles)
      finish <- getCurrentTime
      putStrLn $ sformat ("ok (" % build % ")") (finish `diffUTCTime` start)

    -- Number of frames we want for each animation
    numFrames :: Int
    numFrames = 200

    -- Render every n steps
    renderEvery :: Int -> SlotNr -> Bool
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
    initUtxo = DSL.utxoSingleton (Input (GivenHash 0) 0) (Output Us 1000000)

    largest, randomOff, randomOn :: Hash h World => NamedPolicy h
    largest   = NamedPolicy "largest"   $ simpleCompPolicy (largestFirst)
    randomOff = NamedPolicy "randomOff" $ simpleCompPolicy (random 50 PrivacyModeOff)
    randomOn  = NamedPolicy "randomOn"  $ simpleCompPolicy (random 50 PrivacyModeOn)

    largeThenRandom :: Hash h World => Int -> NamedPolicy h
    largeThenRandom = NamedPolicy "largeThenRandom" .
                        firstThen
                          largestFirst
                          (random 10 PrivacyModeOn)




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
