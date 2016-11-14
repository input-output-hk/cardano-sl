{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Applicative        (empty)
import           Control.Monad              (fail)
import           Control.TimeWarp.Timed     (Millisecond)
import           Data.Aeson                 (decode, fromJSON, json')
import qualified Data.Aeson                 as A
import           Data.Attoparsec.ByteString (eitherResult, many', parseWith)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.HashMap.Strict        as HM
import           Formatting                 (int, sformat, (%))
import           Options.Applicative.Simple (simpleOptions)
import           Universum                  hiding ((<>))
import           Unsafe                     (unsafeFromJust)

import           AnalyzerOptions            (Args (..), argsParser)
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
        HM.fromList . fromMaybe (panic "failed to read txSenderMap") . decode <$>
        LBS.readFile txFile
    logs <- parseFiles files
    let txConfTimes :: HM.HashMap TxId Integer
        txConfTimes = getTxAcceptTimeAvgs confirmationParam logs
        common =
            HM.intersectionWith (\a b -> a - b * 1000) txConfTimes txSenderMap
        average :: Double
        average =
            fromIntegral (sum (toList common)) / fromIntegral (length common)
        averageMsec :: Millisecond
        averageMsec = fromInteger . round $ average / 1000
    print $
        sformat ("Number of transactions which are sent and accepted: " %int) $
        length common
    -- traceShowM $ HM.size logs
    -- traceShowM $ take 10 $ HM.toList logs
    -- traceShowM $ HM.size txSenderMap
    -- traceShowM $ take 10 $ HM.toList txSenderMap
    -- traceShowM $ HM.size txConfTimes
    -- traceShowM $ take 10 $ HM.toList txConfTimes
    -- traceShowM $ length $ (HM.keys txConfTimes) `L.intersect` (HM.keys txSenderMap)
    --LBS.putStr . encode $ getTxAcceptTimeAvgs logs
    print averageMsec

getTxAcceptTimeAvgs :: Word64 -> HM.HashMap FilePath [JLTimedEvent] -> HM.HashMap TxId Integer
getTxAcceptTimeAvgs confirmations fileEvsMap = result
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
        kSl = unflattenSlotId $ flattenSlotId (fromJLSlotId $ jlSlot $ unsafeFromJust mInitB) - confirmations
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
