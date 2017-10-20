{-# LANGUAGE AllowAmbiguousTypes #-}

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

import           Control.Monad.Except      (MonadError (throwError))
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import           Data.List.NonEmpty        ((<|))
import qualified Data.Text                 as T
import           Formatting                (build, int, sformat, (%))
import           Serokell.Util.Text        (listJson)
import           Serokell.Util.Verify      (VerificationRes (..), isVerSuccess)
import           System.Wlog               (WithLogger, logDebug)

import           Pos.Block.Core            (BlockHeader)
import           Pos.Block.Logic.Util      (lcaWithMainChain)
import           Pos.Block.Pure            (VerifyHeaderParams (..), verifyHeader,
                                            verifyHeaders)
import           Pos.Configuration         (HasNodeConfiguration, recoveryHeadersMessage)
import           Pos.Core                  (BlockCount, EpochOrSlot (..),
                                            HasConfiguration, HeaderHash, SlotId (..),
                                            blkSecurityParam, bvdMaxHeaderSize,
                                            difficultyL, epochIndexL, epochOrSlotG,
                                            getChainDifficulty, getEpochOrSlot,
                                            headerHash, headerHashG, headerSlotL,
                                            prevBlockL)
import           Pos.Core.Configuration    (genesisHash)
import           Pos.Crypto                (hash)
import           Pos.DB                    (MonadDBRead)
import qualified Pos.DB.Block              as DB
import qualified Pos.DB.DB                 as DB
import           Pos.Delegation.Cede       (dlgVerifyHeader, runDBCede)
import qualified Pos.GState                as GS
import           Pos.Lrc.Context           (HasLrcContext)
import qualified Pos.Lrc.DB                as LrcDB
import           Pos.Slotting.Class        (MonadSlots (getCurrentSlot))
import           Pos.Util                  (_neHead, _neLast)
import           Pos.Util.Chrono           (NE, NewestFirst (..), OldestFirst (..),
                                            toNewestFirst, toOldestFirst, _NewestFirst,
                                            _OldestFirst)

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
    :: forall ctx m.
    ( HasConfiguration
    , MonadSlots ctx m
    , DB.MonadBlockDB m
    , MonadSlots ctx m
    , HasLrcContext ctx
    )
    => BlockHeader -> m ClassifyHeaderRes
-- Genesis headers seem useless, we can create them by ourselves.
classifyNewHeader (Left _) = pure $ CHUseless "genesis header is useless"
classifyNewHeader (Right header) = fmap (either identity identity) <$> runExceptT $ do
    curSlot <- getCurrentSlot
    tipHeader <- DB.getTipHeader
    let tipEoS = getEpochOrSlot tipHeader
    let newHeaderEoS = getEpochOrSlot header
    let newHeaderSlot = header ^. headerSlotL
    let newHeaderEpoch = header ^. epochIndexL
    let tip = headerHash tipHeader
    maxBlockHeaderSize <- bvdMaxHeaderSize . snd <$> GS.getAdoptedBVFull
    -- First of all we check whether header is from current slot and
    -- ignore it if it's not.
    when (maybe False (newHeaderSlot >) curSlot) $
        throwError $
        CHUseless $ sformat
            ("header is for future slot: our is "%build%
             ", header's is "%build)
            curSlot newHeaderSlot
    when (newHeaderEoS <= tipEoS) $
        throwError $
        CHUseless $ sformat
            ("header's slot "%build%
             " is less or equal than our tip's slot "%build)
            newHeaderEoS tipEoS
        -- If header's parent is our tip, we verify it against tip's header.
    if | tip == header ^. prevBlockL -> do
            leaders <-
                maybe (throwError $ CHUseless "Can't get leaders") pure =<<
                lift (LrcDB.getLeaders newHeaderEpoch)
            let vhp =
                    VerifyHeaderParams
                    { vhpPrevHeader = Just tipHeader
                    -- We don't verify whether header is from future,
                    -- because we already did it above. The principal
                    -- difference is that currently header from future
                    -- leads to 'CHUseless', but if we checked it
                    -- inside 'verifyHeader' it would be 'CHUseless'.
                    -- It's questionable though, maybe we will change
                    -- this decision.
                    , vhpCurrentSlot = Nothing
                    , vhpLeaders = Just leaders
                    , vhpMaxSize = Just maxBlockHeaderSize
                    , vhpVerifyNoUnknown = False
                    }
            case verifyHeader vhp (Right header) of
                VerFailure errors -> throwError $ mkCHRinvalid errors
                _                 -> pass

            dlgHeaderValid <- runDBCede $ runExceptT $ dlgVerifyHeader header
            whenLeft dlgHeaderValid $ throwError . CHInvalid

            pure CHContinues
        -- If header's parent is not our tip, we check whether it's
        -- more difficult than our main chain.
        | tipHeader ^. difficultyL < header ^. difficultyL -> pure CHAlternative
        -- If header can't continue main chain and is not more
        -- difficult than main chain, it's useless.
        | otherwise ->
            pure $ CHUseless $
            "header doesn't continue main chain and is not more difficult"

-- | Result of multiple headers classification.
data ClassifyHeadersRes
    = CHsValid BlockHeader         -- ^ Header list can be applied,
                                   --    LCA child attached.
    | CHsUseless !Text             -- ^ Header is useless.
    | CHsInvalid !Text             -- ^ Header is invalid.

deriving instance Show BlockHeader => Show ClassifyHeadersRes

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
       forall ctx m.
       ( DB.MonadBlockDB m
       , MonadCatch m
       , HasLrcContext ctx
       , MonadSlots ctx m
       , WithLogger m
       , HasConfiguration
       )
    => Bool -- recovery in progress?
    -> NewestFirst NE BlockHeader
    -> m ClassifyHeadersRes
classifyHeaders inRecovery headers = do
    tipHeader <- DB.getTipHeader
    let tip = headerHash tipHeader
    haveOldestParent <- isJust <$> DB.blkGetHeader oldestParentHash
    leaders <- LrcDB.getLeaders oldestHeaderEpoch
    let headersValid =
            isVerSuccess $
            verifyHeaders leaders (headers & _NewestFirst %~ toList)
    mbCurrentSlot <- getCurrentSlot
    let newestHeaderConvertedSlot =
            case newestHeader ^. epochOrSlotG of
                EpochOrSlot (Left e)  -> SlotId e minBound
                EpochOrSlot (Right s) -> s
    if
       | newestHash == headerHash tip ->
             pure $ CHsUseless "Newest hash is the same as our tip"
       | newestHeader ^. difficultyL <= tipHeader ^. difficultyL ->
             pure $ CHsUseless
                 "Newest hash difficulty is not greater than our tip's"
       | Just currentSlot <- mbCurrentSlot,
         not inRecovery,
         newestHeaderConvertedSlot /= currentSlot ->
             pure $ CHsUseless $ sformat
                 ("Newest header is from slot "%build%", but current slot"%
                  " is "%build%" (and we're not in recovery mode)")
                 (newestHeader ^. epochOrSlotG) currentSlot
         -- This check doesn't normally fail. RetrievalWorker
         -- calculates lrc every time before calling this function so
         -- it only fails when oldest header is from the next epoch e'
         -- and somehow lrc didn't calculate data for e' knowing the
         -- last header from e.
       | isNothing leaders ->
             pure $ CHsUseless $
             "Don't know leaders for oldest header epoch " <> pretty headersValid
       | not headersValid ->
             pure $ CHsInvalid "Header chain is invalid"
       | not haveOldestParent ->
             pure $ CHsInvalid
                 "Didn't manage to find block corresponding to parent \
                 \of oldest element in chain (should be one of checkpoints)"
       | otherwise -> fromMaybe uselessGeneral <$> processClassify tipHeader
  where
    newestHeader = headers ^. _NewestFirst . _neHead
    newestHash = headerHash newestHeader
    oldestHeader = headers ^. _NewestFirst . _neLast
    oldestHeaderEpoch = oldestHeader ^. epochIndexL
    oldestParentHash = oldestHeader ^. prevBlockL
    uselessGeneral =
        CHsUseless "Couldn't find lca -- maybe db state updated in the process"
    processClassify tipHeader = runMaybeT $ do
        lift $ logDebug $
            sformat ("Classifying headers: "%listJson) $ map (view headerHashG) headers
        lca <-
            MaybeT . DB.blkGetHeader =<<
            MaybeT (lcaWithMainChain $ toOldestFirst headers)
        let depthDiff :: BlockCount
            depthDiff = getChainDifficulty (tipHeader ^. difficultyL) -
                        getChainDifficulty (lca ^. difficultyL)
        lcaChild <- MaybeT $ pure $
            find (\bh -> bh ^. prevBlockL == headerHash lca) headers
        pure $ if
            | hash lca == hash tipHeader -> CHsValid lcaChild
            | depthDiff < 0 -> error "classifyHeaders@depthDiff is negative"
            | depthDiff > blkSecurityParam ->
                  CHsUseless $
                  sformat ("Difficulty difference of (tip,lca) is "%int%
                           " which is more than blkSecurityParam = "%int)
                          depthDiff blkSecurityParam
            | otherwise -> CHsValid lcaChild

-- | Given a set of checkpoints @c@ to stop at and a terminating
-- header hash @h@, we take @h@ block (or tip if latter is @Nothing@)
-- and fetch the blocks until one of checkpoints is encountered. In
-- case we got deeper than 'recoveryHeadersMessage', we return
-- 'recoveryHeadersMessage' headers starting from the the newest
-- checkpoint that's in our main chain to the newest ones.
getHeadersFromManyTo ::
       ( DB.MonadBlockDB m
       , WithLogger m
       , MonadError Text m
       , HasConfiguration
       , HasNodeConfiguration
       )
    => NonEmpty HeaderHash -- ^ Checkpoints; not guaranteed to be
                           --   in any particular order
    -> Maybe HeaderHash
    -> m (NewestFirst NE BlockHeader)
getHeadersFromManyTo checkpoints startM = do
    logDebug $
        sformat ("getHeadersFromManyTo: "%listJson%", start: "%build)
                checkpoints startM
    validCheckpoints <- noteM "Failed to retrieve checkpoints" $
        nonEmpty . catMaybes <$>
        mapM DB.blkGetHeader (toList checkpoints)
    tip <- GS.getTip
    unless (all ((/= tip) . headerHash) validCheckpoints) $
        throwError "Found checkpoint that is equal to our tip"
    let startFrom = fromMaybe tip startM
        isCheckpoint bh =
            any (\c -> bh ^. headerHashG == c ^. headerHashG) validCheckpoints
        parentIsCheckpoint bh =
            any (\c -> bh ^. prevBlockL == c ^. headerHashG) validCheckpoints
        whileCond bh = not (isCheckpoint bh)
    headers <- noteM "Failed to load headers by depth" . fmap (_NewestFirst nonEmpty) $
        DB.loadHeadersByDepthWhile whileCond recoveryHeadersMessage startFrom
    let newestH = headers ^. _NewestFirst . _neHead
        oldestH = headers ^. _NewestFirst . _neLast
    logDebug $
        sformat ("getHeadersFromManyTo: retrieved headers, oldest is "
                % build % ", newest is " % build) oldestH newestH
    if parentIsCheckpoint oldestH
    then pure headers
    else do
        logDebug $ "getHeadersFromManyTo: giving headers in recovery mode"
        inMainCheckpoints <-
            noteM "Filtered set of valid checkpoints is empty" $
            nonEmpty <$> filterM GS.isBlockInMainChain (toList validCheckpoints)
        logDebug $ "getHeadersFromManyTo: got checkpoints in main chain"
        let lowestCheckpoint =
                maximumBy (comparing getEpochOrSlot) inMainCheckpoints
            loadUpCond _ h = h < recoveryHeadersMessage
        up <- GS.loadHeadersUpWhile lowestCheckpoint loadUpCond
        res <- note "loadHeadersUpWhile returned empty list" $
            _NewestFirst nonEmpty (toNewestFirst $ over _OldestFirst (drop 1) up)
        logDebug $ "getHeadersFromManyTo: loaded non-empty list of headers, returning"
        pure res
  where
    noteM :: (MonadError e n) => e -> n (Maybe a) -> n a
    noteM reason action = note reason =<< action

-- | Given a starting point hash (we take tip if it's not in storage)
-- it returns not more than 'blkSecurityParam' blocks distributed
-- exponentially base 2 relatively to the depth in the blockchain.
getHeadersOlderExp
    :: (HasConfiguration, MonadDBRead m)
    => Maybe HeaderHash -> m (OldestFirst NE HeaderHash)
getHeadersOlderExp upto = do
    tip <- GS.getTip
    let upToReal = fromMaybe tip upto
    -- Using 'blkSecurityParam + 1' because fork can happen on k+1th one.
    (allHeaders :: NewestFirst [] BlockHeader) <-
        -- loadHeadersByDepth always returns nonempty list unless you
        -- pass depth 0 (we pass k+1). It throws if upToReal is
        -- absent. So it either throws or returns nonempty.
        DB.loadHeadersByDepth (blkSecurityParam + 1) upToReal
    let toNE = fromMaybe (error "getHeadersOlderExp: couldn't create nonempty") .
               nonEmpty
    let selectedHashes :: NewestFirst [] HeaderHash
        selectedHashes =
            fmap headerHash allHeaders &
                _NewestFirst %~ selectIndices (twoPowers $ length allHeaders)

    pure . toOldestFirst . (_NewestFirst %~ toNE) $ selectedHashes
  where
    -- For given n, select indices from start so they decrease as
    -- power of 2. Also include last element of the list.
    --
    -- λ> twoPowers 0 ⇒ []
    -- λ> twoPowers 1 ⇒ [0]
    -- λ> twoPowers 5 ⇒ [0,1,3,4]
    -- λ> twoPowers 7 ⇒ [0,1,3,6]
    -- λ> twoPowers 19 ⇒ [0,1,3,7,15,18]
    twoPowers n
        | n < 0 = error $ "getHeadersOlderExp#twoPowers called w/" <> show n
    twoPowers 0 = []
    twoPowers 1 = [0]
    twoPowers n = (takeWhile (< (n - 1)) $ map pred $ 1 : iterate (* 2) 2) ++ [n - 1]
    -- Effectively do @!i@ for any @i@ from the index list applied to
    -- source list. Index list _must_ be increasing.
    --
    -- λ> selectIndices [0, 5, 8] "123456789"
    -- "169"
    -- λ> selectIndices [4] "123456789"
    -- "5"
    selectIndices :: [Int] -> [a] -> [a]
    selectIndices ixs elems  =
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
    :: forall m. (HasConfiguration, MonadDBRead m)
    => HeaderHash -> HeaderHash -> m (Maybe (OldestFirst NE HeaderHash))
getHeadersFromToIncl older newer = runMaybeT . fmap OldestFirst $ do
    -- oldest and newest blocks do exist
    start <- MaybeT $ DB.blkGetHeader newer
    end   <- MaybeT $ DB.blkGetHeader older
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
            nextHeader <- MaybeT $ DB.blkGetHeader nextHash
            guard $ getEpochOrSlot nextHeader > lowerBound
            -- hashes are being prepended so the oldest hash will be the last
            -- one to be prepended and thus the order is OldestFirst
            loadHeadersDo lowerBound (nextHash <| hashes) (nextHeader ^. prevBlockL)
