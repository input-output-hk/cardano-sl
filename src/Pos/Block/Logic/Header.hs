{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Header processing logic.

module Pos.Block.Logic.Header
       ( ClassifyHeaderRes (..)
       , classifyNewHeader
       , ClassifyHeadersRes (..)
       , classifyHeaders
       , getHeadersFromManyTo
       , getHeadersOlderExp
       , getHeadersFromToIncl
       ) where

import           Universum

import           Control.Lens              (_Wrapped)
import           Control.Monad.Except      (MonadError (throwError))
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import           Data.Default              (Default (def))
import           Data.List.NonEmpty        ((<|))
import qualified Data.Text                 as T
import           Formatting                (build, int, sformat, (%))
import           Serokell.Util.Text        (listJson)
import           Serokell.Util.Verify      (VerificationRes (..), isVerSuccess)
import           System.Wlog               (WithLogger, logDebug)

import           Pos.Block.Core            (BlockHeader)
import           Pos.Block.Logic.Util      (lcaWithMainChain, needRecovery)
import           Pos.Block.Pure            (VerifyHeaderParams (..), verifyHeader,
                                            verifyHeaders)
import           Pos.Constants             (blkSecurityParam, genesisHash,
                                            recoveryHeadersMessage)
import           Pos.Core                  (EpochOrSlot (..), HeaderHash, SlotId (..),
                                            difficultyL, epochOrSlotG, getEpochOrSlot,
                                            headerHash, headerHashG, headerSlotL,
                                            prevBlockL, prevBlockL)
import           Pos.Crypto                (hash)
import           Pos.DB                    (MonadDBPure)
import qualified Pos.DB.Block              as DB
import qualified Pos.DB.DB                 as DB
import qualified Pos.DB.GState             as GS
import           Pos.Slotting.Class        (MonadSlots (getCurrentSlot))
import           Pos.Ssc.Class             (SscHelpersClass)
import           Pos.Util                  (_neHead, _neLast)
import           Pos.Util.Chrono           (NE, NewestFirst (..), OldestFirst (..),
                                            toNewestFirst, toOldestFirst)

-- | Result of single (new) header classification.
data ClassifyHeaderRes
    = CHContinues
      -- ^ Header is direct continuation of main chain (i.e. its
      -- parent is our tip).
    | CHAlternative
      -- ^ Header continues main or alternative chain, it's more
      -- difficult than tip.
    | CHUseless !Text
      -- ^ Header is useless.
    | CHInvalid !Text
      -- ^ Header is invalid.
    deriving (Show)

-- | Make `ClassifyHeaderRes` from list of error messages using
-- `CHRinvalid` constructor. Intended to be used with `VerificationRes`.
-- Note: this version forces computation of all error messages. It can be
-- made more efficient but less informative by using head, for example.
mkCHRinvalid :: [Text] -> ClassifyHeaderRes
mkCHRinvalid = CHInvalid . T.intercalate "; "

-- | Classify new header announced by some node. Result is represented
-- as ClassifyHeaderRes type.
classifyNewHeader
    :: (MonadSlots m, SscHelpersClass ssc, MonadDBPure m)
    => BlockHeader ssc -> m ClassifyHeaderRes
-- Genesis headers seem useless, we can create them by ourselves.
classifyNewHeader (Left _) = pure $ CHUseless "genesis header is useless"
classifyNewHeader (Right header) = do
    curSlot <- getCurrentSlot
    tipHeader <- DB.getTipHeader
    let tipEoS = getEpochOrSlot tipHeader
    let newHeaderEoS = getEpochOrSlot header
    let newHeaderSlot = header ^. headerSlotL
    let tip = headerHash tipHeader
    -- First of all we check whether header is from current slot and
    -- ignore it if it's not.
    pure $ if
        -- Checks on slots
        | maybe False (newHeaderSlot >) curSlot ->
            CHUseless $ sformat
               ("header is for future slot: our is "%build%
                ", header's is "%build)
               curSlot newHeaderSlot
        | newHeaderEoS <= tipEoS ->
            CHUseless $ sformat
               ("header's slot "%build%
                " is less or equal than our tip's slot "%build)
               newHeaderEoS tipEoS
        -- If header's parent is our tip, we verify it against tip's header.
        | tip == header ^. prevBlockL ->
            let vhp =
                    def
                    { vhpVerifyConsensus = True
                    , vhpPrevHeader = Just tipHeader
                    }
                verRes = verifyHeader vhp (Right header)
            in case verRes of
                   VerSuccess        -> CHContinues
                   VerFailure errors -> mkCHRinvalid errors
        -- If header's parent is not our tip, we check whether it's
        -- more difficult than our main chain.
        | tipHeader ^. difficultyL < header ^. difficultyL -> CHAlternative
        -- If header can't continue main chain and is not more
        -- difficult than main chain, it's useless.
        | otherwise ->
            CHUseless $
            "header doesn't continue main chain and is not more difficult"

-- | Result of multiple headers classification.
data ClassifyHeadersRes ssc
    = CHsValid (BlockHeader ssc)   -- ^ Header list can be applied,
                                   --    LCA child attached.
    | CHsUseless !Text             -- ^ Header is useless.
    | CHsInvalid !Text             -- ^ Header is invalid.
    deriving (Show)

-- | Classify headers received in response to 'GetHeaders' message.
--
-- * If there are any errors in chain of headers, CHsInvalid is returned.
-- * If chain of headers is a valid continuation or alternative branch,
--    lca child is returned.
-- * If chain of headers forks from our main chain too much, CHsUseless
--    is returned, because paper suggests doing so.
-- * CHsUseless is also returned if we aren't too far behind the current slot
--    (i.e. if 'needRecovery' is false) but the newest header in the list isn't
--    from the current slot. See CSL-177.
classifyHeaders ::
       forall ssc m.
       ( MonadDBPure m
       , SscHelpersClass ssc
       , MonadSlots m
       , MonadCatch m
       , WithLogger m
       )
    => NewestFirst NE (BlockHeader ssc)
    -> m (ClassifyHeadersRes ssc)
classifyHeaders headers = do
    tipHeader <- DB.getTipHeader @ssc
    let tip = headerHash tipHeader
    haveOldestParent <- isJust <$> DB.blkGetHeader @ssc oldestParentHash
    let headersValid = isVerSuccess $
                       verifyHeaders True (headers & _Wrapped %~ toList)
    needRecovery_ <- needRecovery @ssc
    mbCurrentSlot <- getCurrentSlot
    let newestHeaderConvertedSlot =
            case newestHeader ^. epochOrSlotG of
                EpochOrSlot (Left e)  -> SlotId e minBound
                EpochOrSlot (Right s) -> s
    if | not headersValid ->
             pure $ CHsInvalid "Header chain is invalid"
       | not haveOldestParent ->
             pure $ CHsInvalid
                 "Didn't manage to find block corresponding to parent \
                 \of oldest element in chain (should be one of checkpoints)"
       | newestHash == headerHash tip ->
             pure $ CHsUseless "Newest hash is the same as our tip"
       | newestHeader ^. difficultyL <= tipHeader ^. difficultyL ->
             pure $ CHsUseless
                 "Newest hash difficulty is not greater than our tip's"
       | Just currentSlot <- mbCurrentSlot,
         not needRecovery_,
         newestHeaderConvertedSlot /= currentSlot ->
             pure $ CHsUseless $ sformat
                 ("Newest header is from slot "%build%", but current slot"%
                  " is "%build%" (and we're not in recovery mode)")
                 (newestHeader ^. epochOrSlotG) currentSlot
       | otherwise -> fromMaybe uselessGeneral <$> processClassify tipHeader
  where
    newestHeader = headers ^. _Wrapped . _neHead
    newestHash = headerHash newestHeader
    oldestParentHash = headers ^. _Wrapped . _neLast . prevBlockL
    uselessGeneral =
        CHsUseless "Couldn't find lca -- maybe db state updated in the process"
    processClassify tipHeader = runMaybeT $ do
        lift $ logDebug $
            sformat ("Classifying headers: "%listJson) $ map (view headerHashG) headers
        lca <-
            MaybeT . DB.blkGetHeader =<<
            MaybeT (lcaWithMainChain $ toOldestFirst headers)
        let depthDiff = tipHeader ^. difficultyL - lca ^. difficultyL
        lcaChild <- MaybeT $ pure $
            find (\bh -> bh ^. prevBlockL == headerHash lca) headers
        pure $ if
            | hash lca == hash tipHeader -> CHsValid lcaChild
            | depthDiff < 0 -> error "classifyHeaders@depthDiff is negative"
            | depthDiff > blkSecurityParam ->
                  CHsUseless $
                  sformat ("Difficulty difference of (tip,lca) is "%int%
                           " which is more than blkSecurityParam = "%int)
                          depthDiff (blkSecurityParam :: Int)
            | otherwise -> CHsValid lcaChild

-- | Given a set of checkpoints @c@ to stop at and a terminating
-- header hash @h@, we take @h@ block (or tip if latter is @Nothing@)
-- and fetch the blocks until one of checkpoints is encountered. In
-- case we got deeper than 'recoveryHeadersMessage', we return
-- 'recoveryHeadersMessage' headers starting from the the newest
-- checkpoint that's in our main chain to the newest ones.
getHeadersFromManyTo
    :: forall ssc m.
       (DB.MonadBlockDB ssc m, WithLogger m, MonadError Text m)
    => NonEmpty HeaderHash  -- ^ Checkpoints; not guaranteed to be
                            --   in any particular order
    -> Maybe HeaderHash
    -> m (NewestFirst NE (BlockHeader ssc))
getHeadersFromManyTo checkpoints startM = do
    logDebug $
        sformat ("getHeadersFromManyTo: "%listJson%", start: "%build)
                checkpoints startM
    validCheckpoints <- noteM "Failed to retrieve checkpoints" $
        nonEmpty . catMaybes <$>
        mapM (DB.blkGetHeader @ssc) (toList checkpoints)
    tip <- GS.getTip
    unless (all ((/= tip) . headerHash) validCheckpoints) $
        throwError "Found checkpoint that is equal to our tip"
    let startFrom = fromMaybe tip startM
        parentIsCheckpoint bh =
            any (\c -> bh ^. prevBlockL == c ^. headerHashG) validCheckpoints
        whileCond bh = not (parentIsCheckpoint bh)
    headers <- noteM "Failed to load headers by depth" . fmap (_Wrapped nonEmpty) $
        DB.loadHeadersByDepthWhile whileCond recoveryHeadersMessage startFrom
    if parentIsCheckpoint $ headers ^. _Wrapped . _neHead
    then pure headers
    else do
        logDebug $ "getHeadersFromManyTo: giving headers in recovery mode"
        inMainCheckpoints <-
            noteM "Filtered set of valid checkpoints is empty" $ nonEmpty <$>
            filterM (GS.isBlockInMainChain . headerHash)
                    (toList validCheckpoints)
        logDebug $ "getHeadersFromManyTo: got checkpoints in main chain"
        let lowestCheckpoint =
                maximumBy (comparing getEpochOrSlot) inMainCheckpoints
            loadUpCond _ h = h < recoveryHeadersMessage
        up <- GS.loadHeadersUpWhile lowestCheckpoint loadUpCond
        res <- noteM "loadHeadersUpWhile returned empty list" $
            pure $ _Wrapped nonEmpty (toNewestFirst $ over _Wrapped (drop 1) up)
        logDebug $ "getHeadersFromManyTo: loaded non-empty list of headers, returning"
        pure res
  where
    noteM :: (MonadError e n) => e -> n (Maybe a) -> n a
    noteM reason action = note reason =<< action

-- | Given a starting point hash (we take tip if it's not in storage)
-- it returns not more than 'blkSecurityParam' blocks distributed
-- exponentially base 2 relatively to the depth in the blockchain.
getHeadersOlderExp
    :: forall ssc m.
       (MonadDBPure m, SscHelpersClass ssc)
    => Maybe HeaderHash -> m (OldestFirst [] HeaderHash)
getHeadersOlderExp upto = do
    tip <- GS.getTip
    let upToReal = fromMaybe tip upto
    -- Using 'blkSecurityParam + 1' because fork can happen on k+1th one.
    allHeaders <-
        toOldestFirst <$> DB.loadHeadersByDepth @ssc (blkSecurityParam + 1) upToReal
    pure $ OldestFirst $
        selectIndices
            (getOldestFirst (map headerHash allHeaders))
            (twoPowers $ length allHeaders)
  where
    -- Powers of 2
    twoPowers n
        | n < 0 = error $ "getHeadersOlderExp#twoPowers called w/" <> show n
    twoPowers 0 = []
    twoPowers 1 = [0]
    twoPowers n = (takeWhile (< (n - 1)) $ 0 : 1 : iterate (* 2) 2) ++ [n - 1]
    -- Effectively do @!i@ for any @i@ from the index list applied to
    -- source list. Index list should be increasing.
    selectIndices :: [a] -> [Int] -> [a]
    selectIndices elems ixs =
        let selGo _ [] _ = []
            selGo [] _ _ = []
            selGo ee@(e:es) ii@(i:is) skipped
                | skipped == i = e : selGo ee is skipped
                | otherwise = selGo es ii $ succ skipped
        in selGo elems ixs 0

-- CSL-396 don't load all the blocks into memory at once
-- | Given @from@ and @to@ headers where @from@ is older (not strict)
-- than @to@, and valid chain in between can be found, headers in
-- range @[from..to]@ will be found.
getHeadersFromToIncl
    :: forall ssc m .
       (MonadDBPure m, SscHelpersClass ssc)
    => HeaderHash -> HeaderHash -> m (Maybe (OldestFirst NE HeaderHash))
getHeadersFromToIncl older newer = runMaybeT . fmap OldestFirst $ do
    -- oldest and newest blocks do exist
    start <- MaybeT $ DB.blkGetHeader @ssc newer
    end   <- MaybeT $ DB.blkGetHeader @ssc older
    guard $ getEpochOrSlot start >= getEpochOrSlot end
    let lowerBound = getEpochOrSlot end
    if newer == older
    then pure $ one newer
    else loadHeadersDo lowerBound (one newer) $ start ^. prevBlockL
  where
    loadHeadersDo
        :: EpochOrSlot
        -> NonEmpty HeaderHash
        -> HeaderHash
        -> MaybeT m (NonEmpty HeaderHash)
    loadHeadersDo lowerBound hashes nextHash
        | nextHash == genesisHash = mzero
        | nextHash == older = pure $ nextHash <| hashes
        | otherwise = do
            nextHeader <- MaybeT $ (DB.blkGetHeader @ssc) nextHash
            guard $ getEpochOrSlot nextHeader > lowerBound
            -- hashes are being prepended so the oldest hash will be the last
            -- one to be prepended and thus the order is OldestFirst
            loadHeadersDo lowerBound (nextHash <| hashes) (nextHeader ^. prevBlockL)
