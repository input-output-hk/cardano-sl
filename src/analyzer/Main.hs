{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Applicative        (empty)
import           Control.Monad              (fail)
import           Data.Aeson                 (decode, encode, fromJSON, json')
import qualified Data.Aeson                 as A
import           Data.Attoparsec.ByteString (eitherResult, many', parseWith)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.HashMap.Strict        as HM
import qualified Data.List                  as L
import           Options.Applicative.Simple (simpleOptions)
import           Universum                  hiding ((<>))
import           Unsafe                     (unsafeFromJust)

import           AnalyzerOptions            (Args (..), argsParser)
import           Pos.Constants              (k)
import           Pos.Types                  (flattenSlotId, unflattenSlotId)
import           Pos.Util.JsonLog           (JLBlock (..), JLEvent (..),
                                             JLTimedEvent (..), fromJLSlotId)

type TxId = Text
type BlockId = Text

main :: IO ()
main = do
    (Args {..}, ()) <-
        simpleOptions
            "cardano-analyzer"
            "PoS prototype log analyzer"
            "Use it!"
            argsParser
            empty
    (txSenderMap :: HashMap TxId Integer) <-
        HM.fromList .
        fromMaybe (panic "failed to read txSenderMap") . decode <$>
        LBS.readFile txFile
    logs <- parseFiles files
    let txConfTimes :: HM.HashMap TxId Integer
        txConfTimes = getTxAcceptTimeAvgs logs
        common = HM.intersectionWith (-) txConfTimes txSenderMap
        average :: Double
        average = fromIntegral (sum (take 10000 (HM.elems common))) / 1000
    traceShowM $ HM.size txSenderMap
    traceShowM $ take 10 $ HM.toList txSenderMap
    traceShowM $ HM.size txConfTimes
    traceShowM $ take 10 $ HM.toList txConfTimes
    traceShowM $ length $ (HM.keys txConfTimes) `L.intersect` (HM.keys txSenderMap)
    traceShowM $ take 10 $ HM.toList common
    --LBS.putStr . encode $ getTxAcceptTimeAvgs logs
    print average

getTxAcceptTimeAvgs :: HM.HashMap FilePath [JLTimedEvent] -> HM.HashMap TxId Integer
getTxAcceptTimeAvgs fileEvsMap = result
  where
    n = HM.size fileEvsMap
    allEvs = map jlEvent $ mconcat $ HM.elems fileEvsMap
    blocks :: HM.HashMap BlockId JLBlock
    blocks = foldl' addBlock mempty allEvs
    adopted :: HM.HashMap BlockId (HM.HashMap FilePath Integer)
    adopted = HM.foldlWithKey' adPerFile mempty fileEvsMap
    adPerFile m fp = foldl' (addAdopted fp) m . reverse

    adoptedAvgs :: HM.HashMap BlockId Integer
    adoptedAvgs = maximum <$> HM.filter (\m -> HM.size m >= n `div` 2) adopted
    -- avg = (\m -> sum m `div` fromIntegral (HM.size m))

    result :: HM.HashMap TxId Integer
    result = HM.foldlWithKey' impl mempty adoptedAvgs
      where
        impl m id time = case jlTxs <$> kDepth id of
                           Just txs -> foldl' (addTx time) m txs
                           _        -> m
        addTx time m tx = HM.insert tx time m


    kDepth :: BlockId -> Maybe JLBlock
    kDepth initId |isJust mInitB = impl initId
                  |otherwise     = Nothing
      where
        mInitB = initId `HM.lookup` blocks
        kSl = unflattenSlotId $ flattenSlotId (fromJLSlotId $ jlSlot $ unsafeFromJust mInitB) - k
        impl id = HM.lookup id blocks >>=
                      \b -> if (fromJLSlotId $ jlSlot b) <= kSl
                               then return b
                               else impl (jlPrevBlock b)

    addAdopted fp m (JLTimedEvent time (JLAdoptedBlock blockId)) = HM.insert blockId sm' m
      where
        sm = fromMaybe mempty $ HM.lookup blockId m
        sm' = HM.insert fp time sm
    addAdopted _ m _ = m

    addBlock m (JLCreatedBlock block) = HM.insert (jlHash block) block m
    addBlock m _                      = m

parseFiles :: [FilePath] -> IO (HM.HashMap FilePath [JLTimedEvent])
parseFiles = foldM (\m f -> flip (HM.insert f) m <$> parseFile f) mempty

parseFile :: FilePath -> IO [JLTimedEvent]
parseFile f = do
    bytes <- BS.readFile f
    res <- parseWith (pure mempty) (many' $ json' >>= fromJSON') bytes
    case eitherResult res of
      Left s    -> fail $ "Failed reading file " ++ f ++ ":" ++ s
      Right evs -> return evs
  where
    fromJSON' val = case fromJSON val of
                      A.Error e   -> fail e
                      A.Success a -> return a
