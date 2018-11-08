{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Parts of the evaluator that are generic in choice of domain
module InputSelection.Evaluation.Generic (
    -- * Generalize over UTxO representation
    ValueToDouble(..)
  , IsUtxo(..)
    -- * Statistics
    -- ** Per slot
  , SlotStats(..)
  , deriveSlotStats
    -- ** Overall
  , OverallStats(..)
  , initOverallStats
    -- *** Lenses
  , overallFailedPayments
  , overallTxStats
    -- ** Accumulated
  , AccSlotStats(..)
  , initAccSlotStats
  , stepAccStats
    -- * State
  , IntState(..)
  , initIntState
    -- ** Lenses
  , stUtxo
  , stPending
  , stStats
  , stBinSize
    -- * Result of coin selection
  , CoinSelSummary(..)
    -- * Composable and named policies
  , CompSelPolicy(..)
  , simpleCompPolicy
  , firstThen
  , NextPolicy(..)
    -- * Interpreter proper
  , intPolicy
    -- * Bounds
  , Bounds(..)
  , deriveBounds
    -- ** Lenses
  , boundsUtxoHistogram
  , boundsUtxoSize
  , boundsUtxoBalance
  , boundsTxInputs
  , boundsMedianRatio
    -- * Gnuplot support
  , PlotInstr(..)
  , renderPlotInstr
  , writePlotInstrs
    -- * Write statistics to disk
  , writeStats
    -- * Top-level evaluator
  , evaluatePolicy
  , NamedPolicy(..)
  , evaluateUsingEvents
  ) where

import           Universum

import           Control.Lens ((%=), (+=), (.=))
import           Control.Lens.TH (makeLenses)
import           Data.Conduit
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as Text
import           Formatting (build, sformat, (%))
import           System.Directory (createDirectory)
import           System.FilePath ((<.>), (</>))
import qualified System.IO.Error as IO
import           Text.Printf (printf)

import           Cardano.Wallet.Kernel.CoinSelection.Generic
import           Cardano.Wallet.Kernel.Util.StrictStateT

import           InputSelection.Evaluation.Events (Event (..))
import           InputSelection.Evaluation.Options
import           InputSelection.Evaluation.TimeSeries (SlotNr (..), TimeSeries)
import qualified InputSelection.Evaluation.TimeSeries as TS
import           InputSelection.SortedUtxo (SortedUtxo)
import qualified InputSelection.SortedUtxo as Sorted
import           InputSelection.TxStats
import           Util.Histogram (Bin, BinSize (..), Count, Histogram)
import qualified Util.Histogram as Hist
import qualified Util.MultiSet as MultiSet
import           Util.Range (Range (..), Ranges (..), SplitRanges (..))
import qualified Util.Range as Range

{-------------------------------------------------------------------------------
  Generalize over UTxO
-------------------------------------------------------------------------------}

-- | Conversion of values to doubles is used for the histogram computations
class ValueToDouble dom where
  valueToDouble :: Value dom -> Double

-- | The evaluator can be run with policies that require different kinds of
-- internal UTxO representations
class ( PickFromUtxo utxo
      , ValueToDouble (Dom utxo)
      , Buildable utxo
      ) => IsUtxo utxo where
  utxoSize    :: utxo -> Int
  utxoOutputs :: utxo -> [Value (Dom utxo)]
  utxoFromMap :: Map (Input (Dom utxo)) (Output (Dom utxo)) -> utxo
  utxoToMap   :: utxo -> Map (Input (Dom utxo)) (Output (Dom utxo))
  utxoUnion   :: utxo -> utxo -> utxo

instance (StandardDom dom, ValueToDouble dom) => IsUtxo (SortedUtxo dom) where
  utxoSize    = Sorted.size
  utxoOutputs = Sorted.outputs
  utxoFromMap = Sorted.fromMap
  utxoToMap   = Sorted.toMap
  utxoUnion   = Sorted.union

convertUtxo :: (IsUtxo utxo, IsUtxo utxo', Dom utxo ~ Dom utxo')
            => utxo -> utxo'
convertUtxo = utxoFromMap . utxoToMap

{-------------------------------------------------------------------------------
  Statistics about the current value of the system
-------------------------------------------------------------------------------}

-- | Statistics about the /current/ value of the system at the end of a slot
--
-- This information is solely based on the current value of the system, and not
-- affected by the system history. Consequently this does /NOT/ have a 'Monoid'
-- instance.
data SlotStats dom = SlotStats {
      -- | Current UTxO size
      slotUtxoSize      :: !Int

      -- | Current UTxO balance
    , slotUtxoBalance   :: !(Value dom)

      -- | Current UTxO histogram
    , slotUtxoHistogram :: !Histogram
    }

deriveSlotStats :: IsUtxo utxo => BinSize -> utxo -> SlotStats (Dom utxo)
deriveSlotStats binSize utxo = SlotStats {
      slotUtxoSize      = utxoSize              utxo
    , slotUtxoBalance   = utxoBalance           utxo
    , slotUtxoHistogram = utxoHistogram binSize utxo
    }

utxoHistogram :: IsUtxo utxo => BinSize -> utxo -> Histogram
utxoHistogram binSize = Hist.discretize binSize
                      . map valueToDouble
                      . utxoOutputs

{-------------------------------------------------------------------------------
  Overall statistics
-------------------------------------------------------------------------------}

-- | Overall statistics that aren't related to slots
data OverallStats = OverallStats {
      -- | Number of payment requests we failed to satisfy
      _overallFailedPayments :: !Int

      -- | Transaction statistics
    , _overallTxStats        :: !TxStats

      -- | Histogram of all deposits
    , _overallDeposits       :: !Histogram

      -- | Histogram of all payments
    , _overallPayments       :: !Histogram
    }

makeLenses ''OverallStats

initOverallStats :: BinSize -> OverallStats
initOverallStats bz = OverallStats {
      _overallFailedPayments = 0
    , _overallTxStats        = mempty
    , _overallDeposits       = Hist.empty bz
    , _overallPayments       = Hist.empty bz
    }

recordEvent :: forall dom. (CoinSelDom dom, ValueToDouble dom)
            => Event dom -> OverallStats -> OverallStats
recordEvent = \case
    Deposit d ->
      overallDeposits %~ \h ->
        foldl' (\h' -> (`Hist.insert` h') . outValD) h (Map.elems d)
    Pay p ->
      overallPayments %~ \h ->
        foldl' (\h' -> (`Hist.insert` h') . outValD) h p
    NextSlot ->
      identity
  where
    outValD :: Output dom -> Double
    outValD = valueToDouble . outVal

{-------------------------------------------------------------------------------
  Accumulated statistics
-------------------------------------------------------------------------------}

-- | Accumulated statistics at the end of each slot
data AccSlotStats dom = AccSlotStats {
      -- | Maximum UTxO histogram
      --
      -- While not particularly meaningful as statistic to display, this is
      -- useful to determine bounds for rendering.
      _accUtxoMaxHistogram :: !Histogram

      -- | Size of the UTxO over time
    , _accUtxoSize         :: !(TimeSeries Int)

      -- | Total balance of the UTxO over time
    , _accUtxoBalance      :: !(TimeSeries (Value dom))

      -- | Time series of the median change/payment ratio
    , _accMedianRatio      :: !(TimeSeries (Fixed E2))
    }


makeLenses ''AccSlotStats

initAccSlotStats :: BinSize -> AccSlotStats dom
initAccSlotStats binSize = AccSlotStats {
      _accUtxoMaxHistogram = Hist.empty binSize
    , _accUtxoSize         = TS.empty
    , _accUtxoBalance      = TS.empty
    , _accMedianRatio      = TS.empty
    }

-- | Construct statistics for the next step
--
-- For the median ratio timeseries, we use a default value of @-1@ as long as
-- there are no outputs generated yet (since we plot from 0, this will then be
-- not visible).
stepAccStats :: SlotNr
             -> OverallStats
             -> SlotStats    dom
             -> AccSlotStats dom
             -> AccSlotStats dom
stepAccStats slotNr OverallStats{..} SlotStats{..} acc =
    acc & accUtxoMaxHistogram %~ Hist.max slotUtxoHistogram
        & accUtxoSize         %~ TS.insert slotNr slotUtxoSize
        & accUtxoBalance      %~ TS.insert slotNr slotUtxoBalance
        & accMedianRatio      %~ TS.insert slotNr medianChangeRatio
  where
    TxStats{..}       = _overallTxStats
    medianChangeRatio = MultiSet.medianWithDefault (-1) txStatsRatios

{-------------------------------------------------------------------------------
  Interpreter state
-------------------------------------------------------------------------------}

data IntState utxo = IntState {
      -- | Available UTxO
      _stUtxo    :: !utxo

      -- | Pending UTxO
      --
      -- NOTE: Unlike in the wallet, we don't care about the transactions here.
      -- In the simulation, when we "construct a transaction", its inputs are
      -- immediately removed from '_stUtxo', and the change outputs are added
      -- to '_stPending'. Then at the end of a slot we just add all these
      -- new outputs to 'stUxo'.
    , _stPending :: !(Map (Input (Dom utxo)) (Output (Dom utxo)))

      -- | Running statistics
    , _stStats   :: !OverallStats

      -- | Binsize used for histograms
      --
      -- We cannot actually currently change this as we run the interpreter
      -- because `Histogram.max` only applies to histograms wit equal binsizes.
    , _stBinSize :: !BinSize
    }

makeLenses ''IntState

initIntState :: IsUtxo utxo
             => EvalOptions
             -> Map (Input (Dom utxo)) (Output (Dom utxo))
             -> IntState utxo
initIntState EvalOptions{..} utxo = IntState {
      _stUtxo       = utxoFromMap utxo
    , _stPending    = Map.empty
    , _stStats      = initOverallStats utxoBinSize
    , _stBinSize    = utxoBinSize
    }

-- | Convenience function for changing the UTxO of the state
mapIntState :: Dom utxo ~ Dom utxo'
            => (utxo -> utxo')
            -> (BinSize -> OverallStats -> OverallStats)
            -> IntState utxo -> IntState utxo'
mapIntState f g IntState{..} = IntState{
      _stUtxo    = f            _stUtxo
    , _stPending =              _stPending
    , _stStats   = g _stBinSize _stStats
    , _stBinSize =              _stBinSize
    }

{-------------------------------------------------------------------------------
  Result of coin selection
-------------------------------------------------------------------------------}

-- | Variation on 'CoinSelResult' specialized to the evaluator
data CoinSelSummary dom = CoinSelSummary {
      -- | Statistics about the generated transaction
      csSummaryStats     :: TxStats

       -- | Change returning to the wallet
    , csSummaryOurChange :: Map (Input dom) (Output dom)
    }

{-------------------------------------------------------------------------------
  Composable policies
-------------------------------------------------------------------------------}

-- | Composite coin selection policy
data CompSelPolicy utxo m = CompSelPolicy {
      -- | Current policy
      currentPolicy :: CoinSelPolicy utxo m (CoinSelSummary (Dom utxo), utxo)

      -- | Tell the policy we reached the end of a slot
      --
      -- This gives the policy the change to evolve as well as change
      -- representation, if needed
    , nextPolicy    :: NextPolicy utxo m
    }

data NextPolicy utxo m =
    -- | We stick with the same policy for now
    SamePolicy (CompSelPolicy utxo m)

    -- | We change policy (and possibly representation)
  | forall utxo'. (IsUtxo utxo', Dom utxo ~ Dom utxo') =>
      ChangePolicy (utxo -> utxo') (CompSelPolicy utxo' m)

simpleCompPolicy :: forall utxo m.
                    CoinSelPolicy utxo m (CoinSelSummary (Dom utxo), utxo)
                 -> CompSelPolicy utxo m
simpleCompPolicy p = go
  where
    go :: CompSelPolicy utxo m
    go = CompSelPolicy p $ SamePolicy go

firstThen :: forall utxo utxo' m.
             (IsUtxo utxo, IsUtxo utxo', Dom utxo ~ Dom utxo')
          => Int
          -> CoinSelPolicy utxo  m (CoinSelSummary (Dom utxo) , utxo)
          -> CoinSelPolicy utxo' m (CoinSelSummary (Dom utxo'), utxo')
          -> CompSelPolicy utxo  m
firstThen changeAfter p p' = goP changeAfter
  where
    goP :: Int -> CompSelPolicy utxo m
    goP 0 = error "firstThen: expected n > 0"
    goP 1 = CompSelPolicy p $ ChangePolicy convertUtxo (simpleCompPolicy p')
    goP n = CompSelPolicy p $ SamePolicy (goP (n - 1))

{-------------------------------------------------------------------------------
  Interpreter proper
-------------------------------------------------------------------------------}

-- | Interpreter for events, evaluating a policy
--
-- Turns a stream of events into a stream of observations and accumulated
-- statistics.
intPolicy :: forall utxo m. (IsUtxo utxo, Monad m)
          => (SlotNr -> Bool)      -- Slots to render
          -> IntState utxo         -- Initial state
          -> CompSelPolicy utxo m  -- Initial policy
          -> ConduitT (Event (Dom utxo))
                      (SlotNr, OverallStats, SlotStats (Dom utxo))
                      m
                      OverallStats
intPolicy shouldRender =
    loop (SlotNr 0 0 0)
  where
    deriveSlotStats' :: IsUtxo utxo'
                     => SlotNr
                     -> IntState utxo'
                     -> (SlotNr, OverallStats, SlotStats (Dom utxo'))
    deriveSlotStats' slot st = (
          slot
        , st ^. stStats
        , deriveSlotStats (st ^. stBinSize) (st ^. stUtxo)
        )

    loop :: (IsUtxo utxo', Dom utxo ~ Dom utxo')
         => SlotNr
         -> IntState utxo'
         -> CompSelPolicy utxo' m
         -> ConduitT (Event (Dom utxo'))
                     (SlotNr, OverallStats, SlotStats (Dom utxo'))
                     m
                     OverallStats
    loop slot@SlotNr{..} !st policy@CompSelPolicy{..} = do
        mEvent <- await
        case mEvent of
          Nothing ->
            return $ st ^. stStats
          Just event -> do
            (isEndOfSlot, st') <- lift $ flip runStrictStateT st $
                                     intEvent currentPolicy event
            if not isEndOfSlot
              then loop slot st' policy
              else do
                when (shouldRender slot) $ do
                  yield $ deriveSlotStats' slot st'
                case nextPolicy of
                  SamePolicy policy' -> do
                    let slot' = SlotNr { overallSlotNr = overallSlotNr + 1
                                       , policyNr      = policyNr
                                       , policySlotNr  = policySlotNr  + 1
                                       }
                    loop slot' st' policy'
                  ChangePolicy changeRep policy' -> do
                    let slot' = SlotNr { overallSlotNr = overallSlotNr + 1
                                       , policyNr      = policyNr      + 1
                                       , policySlotNr  = 0
                                       }
                        st''  = mapIntState changeRep resetStats st'
                    loop slot' st'' policy'

    -- We reset the overall statistics when changing policy
    resetStats :: BinSize -> OverallStats -> OverallStats
    resetStats bz = const (initOverallStats bz)

    -- Interpret single event
    intEvent :: IsUtxo utxo'
             => CoinSelPolicy utxo' m (CoinSelSummary (Dom utxo'), utxo')
             -> Event (Dom utxo')
             -> StrictStateT (IntState utxo') m Bool
    intEvent f event = do
        stStats %= recordEvent event
        case event of
          Deposit new -> do
            stUtxo %= utxoUnion (utxoFromMap new)
            return False
          NextSlot -> do
            -- TODO: May want to commit only part of the pending transactions
            pending <- use stPending
            stUtxo    %= utxoUnion (utxoFromMap pending)
            stPending .= Map.empty
            return True
          Pay outs -> do
            utxo <- use stUtxo
            mtx  <- lift $ f (NE.fromList outs) utxo
            case mtx of
              Right (CoinSelSummary{..}, utxo') -> do
                stUtxo                   .= utxo'
                stStats . overallTxStats %= mappend csSummaryStats
                stPending                %= Map.union csSummaryOurChange
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
data Bounds dom = Bounds {
      -- | Range of the UTxO
      _boundsUtxoHistogram :: SplitRanges Bin Count

      -- | Range of the UTxO size time series
    , _boundsUtxoSize      :: Ranges TS.OverallSlotNr Int

      -- | Range of the UTxO balance time series
    , _boundsUtxoBalance   :: Ranges TS.OverallSlotNr (Value dom)

      -- | Range of the transaction inputs
    , _boundsTxInputs      :: Ranges Int Int

      -- | Range of the median change/payment time series
    , _boundsMedianRatio   :: Ranges TS.OverallSlotNr (Fixed E2)
    }

deriving instance Show (Value dom) => Show (Bounds dom)

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
deriveBounds :: CoinSelDom dom => OverallStats -> AccSlotStats dom -> Bounds dom
deriveBounds OverallStats{..} AccSlotStats{..} = Bounds {
      _boundsUtxoHistogram = Hist.splitRanges 5 5 _accUtxoMaxHistogram
                           & Range.splitYRange . Range.lo .~ 0
    , _boundsTxInputs      = Hist.range (txStatsNumInputs _overallTxStats)
                           & Range.x . Range.lo .~ 0
                           & Range.y . Range.lo .~ 0
    , _boundsUtxoSize      = TS.range _accUtxoSize
    , _boundsUtxoBalance   = TS.range _accUtxoBalance
                           & Range.y . Range.lo .~ valueZero
                           & Range.y . Range.hi %~ unsafeValueAdjust RoundUp 1.01
    , _boundsMedianRatio   = TS.range _accMedianRatio
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
      --
      -- This is a 'Maybe' because when we reconstruct the plot instructions
      -- (in 'replot'), we cannot recover this information.
    , piFailedPayments :: Maybe Int
    }
  deriving (Show)

-- | Render in gnuplot syntax
renderPlotInstr :: CoinSelDom dom
                => BinSize    -- ^ Bin size (for width of the boxes)
                -> Bounds dom -- ^ Derived bounds
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
    % "set label 1 '" % build % "' at graph 0.95, 0.90 front right\n"
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
    % "plot 'growth'  using 1:4 every ::0::" % build % " notitle axes x1y1 with lines\n"
    % "plot 'balance' using 1:4 every ::0::" % build % " notitle axes x1y2 with lines linecolor rgbcolor 'blue'\n"
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
    % "plot 'ratio' using 1:4 every ::0::" % build % " notitle\n"

    % "unset multiplot\n"
    )

    -- header
    piFrame
    piFilePrefix

    -- current UTxO
    setupSplitAxis
    (bounds ^. boundsUtxoHistogram . Range.splitYRange)
    (maybe "" (sformat ("failed: " % build)) piFailedPayments)
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
writePlotInstrs :: CoinSelDom dom
                => EvalOptions -> FilePath -> Bounds dom -> [PlotInstr] -> IO ()
writePlotInstrs EvalOptions{..} script bounds is = do
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
writeStats :: forall m dom. MonadIO m
           => FilePath      -- ^ Prefix for the files to create
           -> BinSize
           -> ConduitT (SlotNr, OverallStats, SlotStats dom) Void m
                (AccSlotStats dom, [PlotInstr])
writeStats prefix binSize =
    loop (initAccSlotStats binSize) [] 0
  where
    loop :: AccSlotStats dom -- ^ Accumulated slot statistics
         -> [PlotInstr]      -- ^ Accumulated plot instructions
         -> Int              -- ^ Rendered frame counter
         -> ConduitT (SlotNr, OverallStats, SlotStats dom) Void m
              (AccSlotStats dom, [PlotInstr])
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

    go :: Int -> OverallStats -> SlotStats dom -> IO PlotInstr
    go frame OverallStats{..} SlotStats{..} = do
        Hist.writeFile (filepath <.> "histogram") slotUtxoHistogram
        Hist.writeFile (filepath <.> "txinputs") (txStatsNumInputs _overallTxStats)
        return PlotInstr {
            piFilePrefix     = filename
          , piFrame          = frame
          , piFailedPayments = Just _overallFailedPayments
          }
      where
        filename = printf "%08d" frame
        filepath = prefix </> filename

{-------------------------------------------------------------------------------
  Top-level evaluator
-------------------------------------------------------------------------------}

-- | Evaluate a policy
--
-- Returns the accumulated statistics and the plot instructions; we return these
-- separately so that we combine bounds of related plots and draw them with the
-- same scales.
evaluatePolicy :: (IsUtxo utxo, MonadIO m)
               => FilePath             -- ^ Path to write to
               -> (SlotNr -> Bool)     -- ^ Slots to render
               -> CompSelPolicy utxo m -- ^ Policy to evaluate
               -> IntState utxo        -- ^ Initial state
               -> ConduitT () (Event (Dom utxo)) m ()
               -> m (OverallStats, (AccSlotStats (Dom utxo), [PlotInstr]))
evaluatePolicy prefix shouldRender policy initState generator =
      runConduit $
        generator                               `fuse`
        intPolicy shouldRender initState policy `fuseBoth`
        writeStats prefix (initState ^. stBinSize)

data NamedPolicy dom m = forall utxo. (IsUtxo utxo, Dom utxo ~ dom) =>
    NamedPolicy {
        namedPolicyName :: String
      , namedPolicy     :: CompSelPolicy utxo m
      }

-- | Evaluate various input policies given the specified event stream
--
-- We evaluate
--
-- * Largest-first
-- * Random, privacy mode off
-- * Random, privacy mode on
evaluateUsingEvents :: forall dom m. (MonadIO m, MonadCatch m)
                    => EvalOptions
                    -> FilePath              -- ^ Prefix for this event stream
                    -> Map (Input dom) (Output dom) -- ^ Initial UTxO
                    -> [NamedPolicy dom m]   -- ^ Policies to evaluate
                    -> (SlotNr -> Bool)      -- ^ Slots to render
                    -> ConduitT () (Event dom) m () -- ^ Event stream
                    -> m ()
evaluateUsingEvents evalOptions@EvalOptions{..}
                    eventsPrefix
                    initUtxo
                    policies
                    shouldRender
                    events =
    forM_ policies go
  where
    go :: NamedPolicy dom m -> m ()
    go NamedPolicy{..} = do
        go' prefix' namedPolicy `catch` \e ->
          if IO.isAlreadyExistsError e then
            putStrLn $ "Skipping " ++ prefix' ++ " (directory already exists)"
          else
            throwM e
      where
        prefix' = prefix </> (eventsPrefix ++ "-" ++ namedPolicyName)

    go' :: (IsUtxo utxo', Dom utxo' ~ dom)
        => FilePath
        -> CompSelPolicy utxo' m
        -> m ()
    go' prefix' policy = do
        liftIO $
          createDirectory prefix'
        (overallStats, (accStats, plotInstr)) <- evaluatePolicy
          prefix'
          shouldRender
          policy
          (initIntState evalOptions initUtxo)
          events
        liftIO $ do
          TS.writeFile   (prefix' </> "growth")   (accStats     ^. accUtxoSize)
          TS.writeFile   (prefix' </> "balance")  (accStats     ^. accUtxoBalance)
          TS.writeFile   (prefix' </> "ratio")    (accStats     ^. accMedianRatio)
          Hist.writeFile (prefix' </> "deposits") (overallStats ^. overallDeposits)
          Hist.writeFile (prefix' </> "payments") (overallStats ^. overallPayments)
          writePlotInstrs
            evalOptions
            (prefix' </> "mkframes.gnuplot")
            (deriveBounds overallStats accStats)
            plotInstr
