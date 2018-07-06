{-# LANGUAGE TupleSections #-}

module InputSelection.Evaluation.Replot (
    replot
  ) where

import           Universum

import           Data.List (foldl1', isSuffixOf)
import qualified Prelude
import           System.Directory (getDirectoryContents)
import           System.FilePath ((</>))

import           InputSelection.Evaluation.Events (World)
import           InputSelection.Evaluation.Generic
import           InputSelection.Evaluation.Options
import qualified InputSelection.Evaluation.TimeSeries as TS
import           InputSelection.FromGeneric
import qualified Util.Histogram as H
import           Util.Range (Range (..))
import qualified Util.Range as Range
import           UTxO.DSL (GivenHash)

replot :: EvalOptions -> ReplotOptions -> IO ()
replot evalOpts@EvalOptions{..} replotOpts@ReplotOptions{..} = do
    (bounds, instrs) <- recoverInstrs evalOpts replotOpts
    writePlotInstrs
      evalOpts
      (prefix </> replotSubdir </> "mkframes.gnuplot")
      bounds
      instrs

{-------------------------------------------------------------------------------
  Reconstruction
-------------------------------------------------------------------------------}

recoverInstrs :: EvalOptions
              -> ReplotOptions
              -> IO (Bounds (DSL GivenHash World), [PlotInstr])
recoverInstrs EvalOptions{..} ReplotOptions{..} = do
    (prefixes, utxos) <-
      fmap unzip $
        readNumbered (prefix </> replotSubdir) ".histogram" $ \filePrefix fp ->
           (filePrefix,) <$> H.readFile utxoBinSize fp
    txInputs <-
      readNumbered (prefix </> replotSubdir) ".txinputs" $ \_i ->
        H.readFile (H.BinSize 1)
    let maxUtxos    = foldl1' H.max utxos
        maxTxInputs = foldl1' H.max txInputs
    growth  <- TS.readFile (prefix </> replotSubdir </> "growth")
    balance <- TS.readFile (prefix </> replotSubdir </> "balance")
    ratio   <- TS.readFile (prefix </> replotSubdir </> "ratio")
    return ( Bounds {
          _boundsUtxoHistogram = H.splitGivenRanges replotSplits maxUtxos
        , _boundsTxInputs      = H.range maxTxInputs
        , _boundsUtxoSize      = TS.range growth
        , _boundsUtxoBalance   = TS.range balance
        , _boundsMedianRatio   = TS.range ratio
                               & Range.y %~ \(Range _ y) ->
                                   case replotRatioMaxY of
                                     Just y' -> Range 0 y'
                                     Nothing -> Range 0 y
        }
      , map recoverInstr prefixes
      )
  where
    recoverInstr :: String -> PlotInstr
    recoverInstr filePrefix = PlotInstr {
          piFilePrefix     = filePrefix
        , piFrame          = Prelude.read filePrefix
        , piFailedPayments = Nothing
        }

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Parse all numbered files
readNumbered :: FilePath -> String -> (String -> FilePath -> IO a) -> IO [a]
readNumbered dir suffix p = mapM (uncurry p) =<< findNumbered dir suffix

-- | Find all numbered files with the given extension
--
-- Returns the prefix (the number) as well as the full path.
findNumbered :: FilePath -> String -> IO [(String, FilePath)]
findNumbered dir suffix =
    aux <$> getDirectoryContents dir
  where
    aux :: [FilePath] -> [(String, FilePath)]
    aux = sortBy (comparing fst) . mapMaybe checkFile

    checkFile :: FilePath -> Maybe (String, FilePath)
    checkFile file = do
         guard $ suffix `isSuffixOf` file
         let n = take (length file - length suffix) file
         return (n, dir </> file)
