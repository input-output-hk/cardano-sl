-- |

module Test.Pos.Wallet.Web.Util
       ( wpGenBlocks
       , wpGenBlock
       ) where

import           Universum
import           Unsafe                      (unsafeHead)

import           Control.Concurrent.STM      (putTMVar, tryTakeTMVar, writeTVar)
import           Control.Monad.Random.Strict (evalRandT)
import           Test.QuickCheck.Gen         (Gen (MkGen))
import           Test.QuickCheck.Monadic     (pick)

import qualified Data.List.NonEmpty          as NE
import           Pos.Block.Core              (blockHeader)
import           Pos.Block.Types             (Blund)
import           Pos.Context                 (LastKnownHeaderTag, ProgressHeaderTag)
import           Pos.Core                    (BlockCount, headerHashG)
import           Pos.Generator.Block         (genBlocks)
import           Pos.Launcher                (HasConfigurations)
import           Pos.StateLock               (Priority (..), modifyStateLock)
import           Pos.Util.Chrono             (OldestFirst (..))
import           Pos.Util.CompileInfo        (HasCompileInfo)
import           Pos.Util.Util               (HasLens (..), _neLast)

import           Test.Pos.Block.Logic.Util   (EnableTxPayload, InplaceDB,
                                              genBlockGenParams)
import           Test.Pos.Wallet.Web.Mode    (WalletProperty)

wpGenBlocks
    :: (HasCompileInfo, HasConfigurations)
    => Maybe BlockCount
    -> EnableTxPayload
    -> InplaceDB
    -> WalletProperty (OldestFirst [] Blund)
wpGenBlocks blkCnt enTxPayload inplaceDB = do
    params <- genBlockGenParams blkCnt enTxPayload inplaceDB
    g <- pick $ MkGen $ \qc _ -> qc
    lift $ modifyStateLock HighPriority "wpGenBlocks" $ \prevTip -> do
        blunds <- evalRandT (genBlocks params) g
        case NE.nonEmpty $ getOldestFirst blunds of
            Just nonEmptyBlunds -> do
                let tipBlockHeader = nonEmptyBlunds ^. _neLast . _1 . blockHeader
                lastKnownHeader <- view (lensOf @LastKnownHeaderTag)
                atomically $ writeTVar lastKnownHeader (Just tipBlockHeader)
                progressHeader <- view (lensOf @ProgressHeaderTag)
                atomically $ do
                    void $ tryTakeTMVar progressHeader
                    putTMVar progressHeader tipBlockHeader
                pure (tipBlockHeader ^. headerHashG, blunds)
            Nothing -> pure (prevTip, blunds)

wpGenBlock
    :: (HasCompileInfo, HasConfigurations)
    => EnableTxPayload
    -> InplaceDB
    -> WalletProperty Blund
wpGenBlock = fmap (unsafeHead . toList) ... wpGenBlocks (Just 1)
