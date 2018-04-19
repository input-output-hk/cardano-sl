{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}
-- | Pure versions of the Cardano verification functions
module UTxO.Verify
    (
    -- * Verification monad
      Verify -- opaque
    , verify

    -- * Specific verification functions
    , verifyBlocksPrefix
    ) where

import           Universum

import           Control.Lens ((%=), (.=), _Wrapped)
import           Control.Monad.Except
import           Control.Monad.State.Strict (mapStateT)
import           Data.Default (def)
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE
import           System.Wlog

import           Pos.Block.Error
import           Pos.Block.Logic hiding (verifyBlocksPrefix)
import           Pos.Block.Logic.Integrity (verifyBlocks)
import           Pos.Block.Slog hiding (slogVerifyBlocks)
import           Pos.Block.Types
import           Pos.Core
import           Pos.DB.Class (MonadGState (..))
import           Pos.Delegation (DlgUndo (..))
import           Pos.Txp hiding (tgsVerifyBlocks)
import           Pos.Update.Poll
import           Pos.Util (neZipWith4)
import           Pos.Util.Chrono
import           Pos.Util.Lens
import qualified Pos.Util.Modifier as MM
import           Serokell.Util.Verify

{-------------------------------------------------------------------------------
  Verification environment
-------------------------------------------------------------------------------}

-- | Verification environment
--
-- The verification environment contains the data that remains unchanged
-- during verification.
--
-- Unsafe constructor since the various fields must be in agreement with
-- each other. See 'verifyEnv'.
data VerifyEnv = UnsafeVerifyEnv {
      venvInitUtxo         :: Utxo
    , venvInitStakes       :: StakesMap
    , venvInitTotal        :: Coin
    , venvBlockVersionData :: BlockVersionData
    , venvLoggerName       :: LoggerName
    }

verifyEnv' :: HasGenesisData
           => Utxo
           -> BlockVersionData
           -> LoggerName
           -> VerifyEnv
verifyEnv' utxo bvd lname = UnsafeVerifyEnv {
      venvInitUtxo         =                     utxo
    , venvInitStakes       = utxoToStakes        utxo
    , venvInitTotal        = getTotalCoinsInUtxo utxo
    , venvBlockVersionData = bvd
    , venvLoggerName       = lname
    }

verifyEnv :: HasConfiguration => Utxo -> VerifyEnv
verifyEnv utxo =
    verifyEnv'
      utxo
      genesisBlockVersionData
      "verify"

{-------------------------------------------------------------------------------
  Reader monad with access to the verification environment
-------------------------------------------------------------------------------}

newtype WithVerifyEnv a = WithVerifyEnv {
      unWithVerifyEnv :: Reader VerifyEnv a
    }
  deriving (Functor, Applicative, Monad)

withVerifyEnv :: VerifyEnv -> WithVerifyEnv a -> a
withVerifyEnv env a = runReader (unWithVerifyEnv a) env

instance MonadGState WithVerifyEnv where
  gsAdoptedBVData = WithVerifyEnv $ venvBlockVersionData <$> ask

instance HasLoggerName WithVerifyEnv where
  askLoggerName        = WithVerifyEnv $ venvLoggerName <$> ask
  modifyLoggerName f x = WithVerifyEnv $ local f' (unWithVerifyEnv x)
    where
      f' :: VerifyEnv -> VerifyEnv
      f' env = env { venvLoggerName = f (venvLoggerName env) }

{-------------------------------------------------------------------------------
  Verification monad

  The verification monad is set up to facilitate 'verifyToil', which seems to
  be the workhorse of verification (verifying transactions inside a block).

  NOTE: Ideally we'd hide 'HasConfiguration' here in the same way that we did
  for 'Translate', but this is made impossible by the superclass constraint of
  'MonadUtxoRead'.
-------------------------------------------------------------------------------}

newtype Verify e a = Verify {
      --    StateT st (ErrorT e (Reader env)) a
      -- == st -> env -> Either e (a, st)
      unVerify :: StateT (GlobalToilState, [LogEvent]) (ExceptT e WithVerifyEnv) a
    }
  deriving (Functor, Applicative, Monad)

-- | Run the verifier
--
-- Returns the result of verification as well as the final UTxO.
verify :: HasConfiguration => Utxo -> Verify e a -> Either e (a, Utxo)
verify utxo ma =
    second finalUtxo <$>
    verify' (defGlobalToilState, []) (verifyEnv utxo) ma
  where
    finalUtxo :: (GlobalToilState, [LogEvent]) -> Utxo
    finalUtxo (gts, _) = MM.modifyMap (gts ^. gtsUtxoModifier) utxo

verify' :: (GlobalToilState, [LogEvent])
        -> VerifyEnv
        -> Verify e a
        -> Either e (a, (GlobalToilState, [LogEvent]))
verify' st env ma = withVerifyEnv env
                  $ runExceptT
                  $ runStateT (unVerify ma) st

deriving instance MonadGState     (Verify e)
deriving instance (MonadError e)  (Verify e)

-- Possible due to HasLoggerName instance for 'StateT'' in "Pos.Util.Orphans"
deriving instance HasLoggerName (Verify e)

instance CanLog (Verify e) where
  dispatchMessage lname sev txt = Verify $
      _2 %= (logEvent :)
    where
      logEvent :: LogEvent
      logEvent = LogEvent lname sev txt

mapVerifyErrors :: (e -> e') -> Verify e a -> Verify e' a
mapVerifyErrors f (Verify ma) = Verify $ mapStateT (withExceptT f) ma

{-------------------------------------------------------------------------------
  Block verification

  There appears to be only a single "pure" block verification function
  (requiring only HasConfiguration): 'Pos.Block.Logic.Integrity.verifyBlocks'.
  Unfortunately, it seems this really only verifies the block envelope (maximum
  block size, unknown attributes, that sort of thing), not the transactions
  contained within. There is also

  1. 'Pos.Block.Logic.VAR.verifyBlocksPrefix'
     Requires 'MonadBlockVerify'.

  2. 'Pos.Block.Slog.Logic.slogVerifyBlocks'
     Requires 'MonadSlogVerify'.
     Called by (1) and calls 'Pos.Block.Logic.Integrity.verifyBlocks'.
     Doesn't seem to do any additional verification itself.

  3. 'Pos.Ssc.Logic.VAR.sscVerifyBlocks'
     Requires 'SscGlobalVerifyMode'.
     Called by (1).
     I think this only verifies SSC stuff (shared seed computation).

  4. 'Pos.Txp.Settings.Global.tgsVerifyBlocks'
     Requires 'TxpGlobalVerifyMode'.
     Called by (1).
     This is actually just a record selector for 'TxpGlobalSettings'; see (5).
     Documented as "Verify a chain of payloads from blocks and return txp undos
     for each payload".

  5. 'Pos.Txp.Logic.Global.verifyBlocks'
     Requires 'TxpGlobalVerifyMode'.
     It seems this is the /only/ instantiation of 'tgsVerifyBlocks' (in the core
     libraries at least); thus, also called by (1).

  6. 'Pos.Delegation.Logic.VAR.dlgVerifyBlocks'
      No constraint synonym, but requires 'MonadDBRead' and 'MonadIO'.
      According to its header comment, verifies delegation logic.

  None of these are really callable in a pure context; all of them rely on
  'MonadIO', either directly, or else indirectly through the superclass
  constraints of 'MonadDBRead'. The most important one is (5).
-------------------------------------------------------------------------------}

-- | Verify new blocks. If parent of the first block is not our tip,
-- verification fails. All blocks must be from the same epoch.  This
-- function checks literally __everything__ from blocks, including
-- header, body, extra data, etc.
--
-- LRC must be already performed for the epoch from which blocks are.
--
-- Adapted from 'Pos.Block.Logic.VAR.verifyBlocksPrefix'.
--
-- Differences from original:
--
-- * Expected tip passed as argument instead of 'getTip'
-- * Uses adapted 'slogVerifyBlocks' (see below)
-- * SSC verification skipped
-- * Transaction verification not configurable through 'TxpGlobalSettings',
--   but instead uses (adapted) 'tgsVerifyBlocks' directly
-- * Delegation verification ('dlgVerifyBlocks') is skipped
-- * Update proposal verification ('usVerifyBlocks') is skipped
-- * Use hardcoded 'dataMustBeKnown' (instead of deriving from 'adoptedBV')
--
-- Since we skip some verification steps, parts of the 'Undo' values we return
-- are also dummies. Update verification also constructs a 'PollModifier' which
-- is returned by the original; since we don't call this, we don't return it.
--
-- NOTE: If skipping these two parts of verification is not okay, or if we need
-- proper 'Undo' values for those, we'd need to find ways to " purify " the
-- corresponding functions from the Cardano core. This didn't look very easy
-- so I skipped it for now.
verifyBlocksPrefix
    :: HasConfiguration
    => HeaderHash    -- ^ Expected tip
    -> Maybe SlotId  -- ^ Current slot
    -> SlotLeaders   -- ^ Slot leaders for this epoch
    -> LastBlkSlots  -- ^ Last block slots
    -> OldestFirst NE Block
    -> Verify VerifyBlocksException (OldestFirst NE Undo)
verifyBlocksPrefix tip curSlot leaders lastSlots blocks = do
    when (tip /= blocks ^. _Wrapped . _neHead . prevBlockL) $
        throwError $ VerifyBlocksError "the first block isn't based on the tip"

    -- And then we run verification of each component.

    -- Verify block envelope
    slogUndos <- mapVerifyErrors VerifyBlocksError $
                   slogVerifyBlocks curSlot leaders lastSlots blocks

    -- We skip SSC verification
    {-
    _ <- withExceptT (VerifyBlocksError . pretty) $
        ExceptT $ sscVerifyBlocks (map toSscBlock blocks)
    -}

    -- Verify transactions
    txUndo <- mapVerifyErrors (VerifyBlocksError . pretty) $
        tgsVerifyBlocks $ map toTxpBlock blocks

    -- Skip delegation verification
    {-
    pskUndo <- withExceptT VerifyBlocksError $ dlgVerifyBlocks blocks
    -}
    let pskUndo :: OldestFirst NE DlgUndo
        pskUndo = map (const (DlgUndo [] HS.empty)) slogUndos

    -- Skip update verification
    {-
    (pModifier, usUndos) <- withExceptT (VerifyBlocksError . pretty) $
        ExceptT $ usVerifyBlocks dataMustBeKnown (map toUpdateBlock blocks)
    -}
    let usUndos :: OldestFirst NE USUndo
        usUndos = map (const def) slogUndos

    -- Eventually we do a sanity check just in case and return the result.
    --
    -- NOTE: Original compared txUndo with pksUndo but that is a bit pointless
    -- with our current simplifications.
    when (length txUndo /= length slogUndos) $
        throwError $ VerifyBlocksError
        "Internal error of verifyBlocksPrefix: lengths of undos don't match"

    pure ( OldestFirst $ neZipWith4 Undo
               (getOldestFirst txUndo)
               (getOldestFirst pskUndo)
               (getOldestFirst usUndos)
               (getOldestFirst slogUndos) )

-- | Verify everything from block that is not checked by other components.
-- All blocks must be from the same epoch.
--
-- Adapted from 'Pos.Block.Slog.Logic.slogVerifyBlocks'.
--
-- Differences from original:
--
-- * Current slot passed as an argument instead of 'getCurrentSlot'
-- * Slot leaders passed as an argument instead of 'getLeadersForEpoch'
-- * Last slots are passed in instead of 'getLastSlots'
-- * Uses 'gsAdoptedBVData' instead of 'getAdoptedBVFull'
-- * Use hard-coded 'dataMustBeKnown' (instead of deriving this from 'adoptedBV')
slogVerifyBlocks
    :: HasConfiguration
    => Maybe SlotId  -- ^ Current slot
    -> SlotLeaders   -- ^ Slot leaders for this epoch
    -> LastBlkSlots  -- ^ Last block slots
    -> OldestFirst NE Block
    -> Verify Text (OldestFirst NE SlogUndo)
slogVerifyBlocks curSlot leaders lastSlots blocks = do
    adoptedBVD <- gsAdoptedBVData

    -- We take head here, because blocks are in oldest first order and
    -- we know that all of them are from the same epoch. So if there
    -- is a genesis block, it must be head and only head.
    case blocks ^. _Wrapped . _neHead of
        (Left block) ->
            when (block ^. genBlockLeaders /= leaders) $
            throwError "Genesis block leaders don't match with LRC-computed"
        _ -> pass
    verResToMonadError formatAllErrors $
        verifyBlocks curSlot dataMustBeKnown adoptedBVD leaders blocks

    -- Here we need to compute 'SlogUndo'. When we add apply a block,
    -- we can remove one of the last slots stored in
    -- 'BlockExtra'. This removed slot must be put into 'SlogUndo'.
    let toFlatSlot = fmap (flattenSlotId . view mainBlockSlot) . rightToMaybe
    -- these slots will be added if we apply all blocks
    let newSlots = mapMaybe toFlatSlot (toList blocks)
    let combinedSlots :: OldestFirst [] FlatSlotId
        combinedSlots = lastSlots & _Wrapped %~ (<> newSlots)
    -- these slots will be removed if we apply all blocks, because we store
    -- only limited number of slots
    let removedSlots :: OldestFirst [] FlatSlotId
        removedSlots =
            combinedSlots & _Wrapped %~
            (take $ length combinedSlots - fromIntegral blkSecurityParam)
    -- Note: here we exploit the fact that genesis block can be only 'head'.
    -- If we have genesis block, then size of 'newSlots' will be less than
    -- number of blocks we verify. It means that there will definitely
    -- be 'Nothing' in the head of the result.
    --
    -- It also works fine if we store less than 'blkSecurityParam' slots.
    -- In this case we will use 'Nothing' for the oldest blocks.
    let slogUndo :: OldestFirst [] (Maybe FlatSlotId)
        slogUndo =
            map Just removedSlots & _Wrapped %~
            (replicate (length blocks - length removedSlots) Nothing <>)
    -- NE.fromList is safe here, because it's obvious that the size of
    -- 'slogUndo' is the same as the size of 'blocks'.
    return $ over _Wrapped NE.fromList $ map SlogUndo slogUndo

-- | Verify block transactions
--
-- Adapted from 'Pos.Txp.Logic.Global.verifyBlocks'.
--
-- Differences from original:
--
-- * 'verifyAllIsKnown' hardcoded ('dataMustBeKnown')
-- * Does everything in a pure monad.
--   I don't fully grasp the consequences of this.
tgsVerifyBlocks
    :: HasConfiguration
    => OldestFirst NE TxpBlock
    -> Verify ToilVerFailure (OldestFirst NE TxpUndo)
tgsVerifyBlocks newChain = do
    bvd <- gsAdoptedBVData
    let epoch = NE.last (getOldestFirst newChain) ^. epochIndexL
    let verifyPure :: [TxAux] -> Verify ToilVerFailure TxpUndo
        verifyPure = nat . verifyToil bvd epoch dataMustBeKnown
    mapM (verifyPure . convertPayload) newChain
  where
    convertPayload :: TxpBlock -> [TxAux]
    convertPayload (ComponentBlockMain _ payload) = flattenTxPayload payload
    convertPayload (ComponentBlockGenesis _)      = []
    nat :: forall e a. ExceptT e UtxoM a -> Verify e a
    nat action =
        Verify $ do
            baseUtxo <- lift . lift $ venvInitUtxo <$> WithVerifyEnv ask
            utxoModifier <- use (_1 . gtsUtxoModifier)
            case runUtxoM utxoModifier (utxoToLookup baseUtxo) $
                 runExceptT action of
                (Left err, _) -> throwError err
                (Right res, newModifier) ->
                    res <$ (_1 . gtsUtxoModifier .= newModifier)

-- | Check all data
--
-- In the regular verification code in various places are checks to see whether
-- we expect blocks to contain data we don't know about (if version numbers
-- mismatch). Since we generate all our own blocks in the tests, this is not
-- relevant here.
dataMustBeKnown :: Bool
dataMustBeKnown = True
