{-# LANGUAGE TypeFamilies #-}

module Pos.Ssc.GodTossing.Instance
       ( -- * Instances
         -- ** instance SscHelpersClass
       ) where

import           Universum

import           Pos.Binary.Core                    ()
import           Pos.Binary.GodTossing              ()
import           Pos.Core                           (HasConfiguration)
import           Pos.Ssc.Class.Helpers              (SscHelpersClass (..))
import           Pos.Ssc.Core                       (defaultSscPayload,
                                                     stripSscPayload)
import           Pos.Ssc.GodTossing.Functions       (sanityChecksSscPayload)
import           Pos.Ssc.VerifyError                (SscVerifyError (..))

instance HasConfiguration => SscHelpersClass where
    sscVerifyPayload = sanityChecksSscPayload
    sscStripPayload = stripSscPayload
    sscDefaultPayload = defaultSscPayload
    sscIsCriticalError =
        \case
            TossInternallError {} -> True
            _ -> False
