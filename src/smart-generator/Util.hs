module Util where

import           Data.Text  (Text)
import           Formatting (build, fixed, int, sformat, (%))
import           Universum

import           Pos.Types  (TxId)

tpsCsvFile :: FilePath
tpsCsvFile = "smart-gen-tps.csv"

verifyCsvFile :: Int -> FilePath
verifyCsvFile rnd = "smart-gen-verifications-round" ++ show rnd ++ ".csv"

verifyCsvHeader, tpsCsvHeader :: Text
tpsCsvHeader = "global_time,round_tps,real_tps\n"
verifyCsvHeader = "transaction_id,sending_ts,verification_ts\n"

tpsCsvFormat :: (Double, Double, Double) -> Text
tpsCsvFormat (gtime, roundTPS, realTPS) =
    sformat (fixed 2%","%fixed 2%","%fixed 2%"\n") gtime roundTPS realTPS

verifyCsvFormat :: (TxId, Word64, Word64) -> Text
verifyCsvFormat (txId, sendTs, verifyTs) =
    sformat (build%","%int%","%int%"\n") txId sendTs verifyTs

