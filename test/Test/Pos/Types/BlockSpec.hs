-- | Specification of Pos.Types.Block.

module Test.Pos.Types.BlockSpec
       ( spec
       ) where

import           Test.Hspec            (Spec)
import           Universum

import           Pos.Ssc.Class.Helpers (SscHelpersClass (..))
import           Pos.Ssc.Class.Types   (Ssc (..))
import qualified Pos.Types             as T

spec :: Spec
spec = pure ()

validateGoodHeader
    :: (SscHelpersClass ssc, T.BiSsc ssc)
    => T.VerifyBlockParams ssc -> T.BlockHeader ssc -> Bool
validateGoodHeader = undefined
