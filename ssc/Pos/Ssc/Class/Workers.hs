{-# LANGUAGE AllowAmbiguousTypes #-}

-- | General purpose workers for @SSC@ storage.

module Pos.Ssc.Class.Workers
       ( SscWorkersClass(..)
       ) where

import           Pos.Communication.Protocol (OutSpecs, WorkerSpec)
import           Pos.Lrc.Consumer           (LrcConsumer)
import           Pos.Lrc.Mode               (LrcMode)
import           Pos.Ssc.Class.Types        (Ssc (..))
import           Pos.Ssc.Mode               (SscMode)

-- | Class for @SSC@ workers.
class Ssc ssc => SscWorkersClass ssc where

    -- | All workers specific to SSC.
    sscWorkers :: SscMode ssc ctx m
               => ([WorkerSpec m], OutSpecs)

    -- | LRC consumers. Note that this uses 'LrcMode', not 'SscMode'.
    sscLrcConsumers :: LrcMode ssc ctx m
                    => [LrcConsumer m]
