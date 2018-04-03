{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Functions that validate headers coming from the network and retrieve headers
-- from the DB.

module Pos.Block.Logic.Header
       ( ClassifyHeaderRes (..)
       , classifyNewHeader
       , ClassifyHeadersRes (..)
       , classifyHeaders

       , GetHeadersFromManyToError (..)
       , getHeadersFromManyTo
       , getHeadersOlderExp
       , GetHashesRangeError (..)
       , getHashesRange
       ) where

import           Universum hiding (elems)

import           Control.Lens (to)
import           Control.Monad.Except (MonadError (throwError))
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import qualified Data.Text as T
import qualified Data.List as List (last)
import qualified Data.List.NonEmpty as NE (toList)
import           Formatting (build, int, sformat, (%))
import           Serokell.Util.Text (listJson)
import           Serokell.Util.Verify (VerificationRes (..), isVerSuccess)
import           System.Wlog (WithLogger, logDebug)
import           UnliftIO (MonadUnliftIO)

import           Pos.Block.Logic.Integrity (VerifyHeaderParams (..), verifyHeader, verifyHeaders)
import           Pos.Block.Logic.Util (lcaWithMainChain)
import           Pos.Core (BlockCount, EpochOrSlot (..), HeaderHash, SlotId (..), HasProtocolMagic,
                           blkSecurityParam, bvdMaxHeaderSize, difficultyL, epochIndexL,
                           epochOrSlotG, getChainDifficulty, getEpochOrSlot, headerHash,
                           headerHashG, headerSlotL, prevBlockL,
                           HasProtocolConstants, HasGenesisHash)
import           Pos.Core.Block (BlockHeader (..))
import           Pos.Crypto (hash)
import           Pos.DB (MonadDBRead)
import qualified Pos.DB.Block.Load as DB
import qualified Pos.DB.BlockIndex as DB
import qualified Pos.DB.GState.Common as GS (getTip)
import           Pos.Delegation.Cede (dlgVerifyHeader, runDBCede)
import qualified Pos.GState.BlockExtra as GS
import           Pos.Lrc.Context (HasLrcContext)
import qualified Pos.Lrc.DB as LrcDB
import           Pos.Slotting.Class (MonadSlots (getCurrentSlot))
import qualified Pos.Update.DB as GS (getAdoptedBVFull)
import           Pos.Util (buildListBounds, _neHead, _neLast)
import           Pos.Util.Chrono (NE, NewestFirst (..), OldestFirst (..), toNewestFirst,
                                  toOldestFirst, _NewestFirst, _OldestFirst)

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
    ( MonadSlots ctx m
    , MonadDBRead m
    , MonadUnliftIO m
    , MonadSlots ctx m
    , HasLrcContext ctx
    , HasProtocolMagic
    )
    => BlockHeader -> m ClassifyHeaderRes
-- Genesis headers seem useless, we can create them by ourselves.
classifyNewHeader (BlockHeaderGenesis _) = pure $ CHUseless "genesis header is useless"
classifyNewHeader (BlockHeaderMain header) = fmap (either identity identity) <$> runExceptT $ do
    curSlot <- getCurrentSlot
    tipHeader <- lift DB.getTipHeader
    let tipEoS = getEpochOrSlot tipHeader
    let newHeaderEoS = getEpochOrSlot header
    let newHeaderSlot = header ^. headerSlotL
    let newHeaderEpoch = header ^. epochIndexL
    let tip = headerHash tipHeader
    maxBlockHeaderSize <- bvdMaxHeaderSize . snd <$> lift GS.getAdoptedBVFull
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
                lift (LrcDB.getLeadersForEpoch newHeaderEpoch)
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
            case verifyHeader vhp (BlockHeaderMain header) of
                VerFailure errors -> throwError $ mkCHRinvalid (NE.toList errors)
                _                 -> pass

            dlgHeaderValid <- lift $ runDBCede $ dlgVerifyHeader header
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

deriving instance Show ClassifyHeadersRes

-- | Classify headers received in response to 'GetHeaders' message.
--
-- * If there are any errors in chain of headers, CHsInvalid is returned.
-- * If chain of headers is a valid continuation or alternative branch,
--    lca child is returned.
-- * If chain of headers forks from our main chain too much, CHsUseless
--    is returned, because paper suggests doing so.
-- * CHsUseless is also returned if we aren't too far behind the current slot,
--    but the newest header in the list isn't
--    from the current slot. See CSL-177.
classifyHeaders ::
       forall ctx m.
       ( MonadDBRead m
       , MonadCatch m
       , HasLrcContext ctx
       , MonadSlots ctx m
       , WithLogger m
       , HasProtocolConstants
       , HasProtocolMagic
       )
    => Bool -- recovery in progress?
    -> NewestFirst NE BlockHeader
    -> m ClassifyHeadersRes
classifyHeaders inRecovery headers = do
    tipHeader <- DB.getTipHeader
    let tip = headerHash tipHeader
    haveOldestParent <- isJust <$> DB.getHeader oldestParentHash
    leaders <- LrcDB.getLeadersForEpoch oldestHeaderEpoch
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
            sformat ("Classifying headers (newest first): "%buildListBounds) $
                getNewestFirst $ map (view headerHashG) headers
        lca <-
            MaybeT . DB.getHeader =<<
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


data GetHeadersFromManyToError = GHFBadInput Text deriving (Show,Generic)

-- | Given a set of checkpoints @c@ to stop at and a terminating
-- header hash @h@, we take @h@ block (or tip if latter is @Nothing@)
-- and fetch the blocks until one of checkpoints is encountered. In
-- case we got deeper than the limit (if given) we return
-- that number of headers starting from the the newest
-- checkpoint that's in our main chain to the newest ones.
getHeadersFromManyTo ::
       ( MonadDBRead m
       , WithLogger m
       )
    => Maybe Word          -- ^ Optional limit on how many to bring in.
    -> NonEmpty HeaderHash -- ^ Checkpoints; not guaranteed to be
                           --   in any particular order
    -> Maybe HeaderHash
    -> m (Either GetHeadersFromManyToError (NewestFirst NE BlockHeader))
getHeadersFromManyTo mLimit checkpoints startM = runExceptT $ do
    logDebug $
        sformat ("getHeadersFromManyTo: "%listJson%", start: "%build)
                checkpoints startM
    tip <- lift DB.getTipHeader
    let tipHash = headerHash tip
    let startHash = maybe tipHash headerHash startM

    -- This filters out invalid/unknown checkpoints also.
    inMainCheckpoints <-
        maybe (throwLocal "no checkpoints are in the main chain") pure =<<
        lift (nonEmpty <$> filterM GS.isBlockInMainChain (toList checkpoints))
    let inMainCheckpointsHashes = map headerHash inMainCheckpoints
    when (tipHash `elem` inMainCheckpointsHashes) $
        throwLocal "found checkpoint that is equal to our tip"
    logDebug $ "getHeadersFromManyTo: got checkpoints in main chain"

    if (tip ^. prevBlockL . headerHashG) `elem` inMainCheckpointsHashes
        -- Optimization for the popular case "just get me the newest
        -- block, i know the previous one".
        then pure $ one tip
        -- If optimization doesn't apply, just iterate down-up
        -- starting with the newest header.
        else do
            newestCheckpoint <-
                maximumBy (comparing getEpochOrSlot) . catMaybes <$>
                lift (mapM DB.getHeader (toList inMainCheckpoints))
            let loadUpCond (headerHash -> curH) h =
                    curH /= startHash && maybe True ((<) h) mLimitInt
            up <- lift $ GS.loadHeadersUpWhile newestCheckpoint loadUpCond
            res <-
                maybe (throwLocal "loadHeadersUpWhile returned empty list") pure $
                _NewestFirst nonEmpty $
                toNewestFirst $ over _OldestFirst (drop 1) up
            logDebug $ "getHeadersFromManyTo: loaded non-empty list of headers, returning"
            pure res
  where
    mLimitInt :: Maybe Int
    mLimitInt = fromIntegral <$> mLimit

    throwLocal = throwError . GHFBadInput

-- | Given a starting point hash (we take tip if it's not in storage)
-- it returns not more than 'blkSecurityParam' blocks distributed
-- exponentially base 2 relatively to the depth in the blockchain.
getHeadersOlderExp
    :: ( MonadDBRead m, HasProtocolConstants, HasGenesisHash )
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


data GetHashesRangeError = GHRBadInput Text deriving (Show,Generic)

-- Throws 'GetHashesRangeException'.
throwGHR :: Monad m => Text -> ExceptT GetHashesRangeError m a
throwGHR = throwError . GHRBadInput

-- | Given optional @depthLimit@, @from@ and @to@ headers where @from@
-- is older (not strict) than @to@, and valid chain in between can be
-- found, headers in range @[from..to]@ will be found. If the number
-- of headers in the chain (which should be returned) is more than
-- @depthLimit@, error will be thrown.
getHashesRange ::
       forall m. (MonadDBRead m)
    => Maybe Word
    -> HeaderHash
    -> HeaderHash
    -> m (Either GetHashesRangeError (OldestFirst NE HeaderHash))
getHashesRange depthLimitM older newer | older == newer = runExceptT $ do
    unlessM (isJust <$> lift (DB.getHeader newer)) $
        throwGHR "can't find newer-older header"
    whenJust depthLimitM $ \depthLimit ->
        when (depthLimit < 1) $
        throwGHR $
        sformat ("depthLimit is "%int%
                 ", we can't return the single requested header "%build)
                depthLimit
                newer
    pure $ OldestFirst $ one newer
getHashesRange depthLimitM older newer = runExceptT $ do
    let fromMaybeM :: Text
                   -> ExceptT GetHashesRangeError m (Maybe x)
                   -> ExceptT GetHashesRangeError m x
        fromMaybeM r m = maybe (throwGHR r) pure =<< m
    -- oldest and newest blocks do exist
    newerHd <- fromMaybeM "can't retrieve newer header" $ DB.getHeader newer
    olderHd <- fromMaybeM "can't retrieve older header" $ DB.getHeader older
    let olderD = olderHd ^. difficultyL
    let newerD = newerHd ^. difficultyL

    -- Proving newerD >= olderD
    let newerOlderF = "newer: "%build%", older: "%build
    when (newerD == olderD) $
        throwGHR $
        sformat ("newer and older headers have "%
                 "the same difficulty, but are not equal. "%newerOlderF)
                newerHd olderHd
    when (newerD < olderD) $
        throwGHR $
        sformat ("newer header is less dificult than older one. "%
                 newerOlderF)
                newerHd olderHd

    -- How many epochs does this range cross.
    let genDiff :: Int
        genDiff = fromIntegral $ newerHd ^. epochIndexL - olderHd ^. epochIndexL
    -- Number of blocks is difficulty difference + number of genesis blocks.
    -- depthDiff + 1 is length of a list we'll return.
    let depthDiff :: Word
        depthDiff = fromIntegral $ genDiff + fromIntegral (newerD - olderD)

    whenJust depthLimitM $ \depthLimit ->
        when (depthDiff + 1 > depthLimit) $
        throwGHR $
        sformat ("requested "%int%" headers, but depthLimit is "%
                 int%". Headers: "%newerOlderF)
                depthDiff
                depthLimit
                newerHd olderHd

    -- We load these depthDiff blocks.
    let cond curHash _depth = curHash /= newer

    -- This is [oldest..newest) headers, oldest first
    allExceptNewest <- lift $ GS.loadHashesUpWhile older cond

    -- Sometimes we will get an empty list, if we've just switched the
    -- branch (after first checks are performed here) and olderHd is
    -- no longer in the main chain.
    -- CSL-1950 We should use snapshots here.
    when (null $ allExceptNewest ^. _OldestFirst) $ throwGHR $
        "loaded 0 headers though checks passed. " <>
        "May be (very rare) concurrency problem, just retry"

    -- It's safe to use 'unsafeLast' here after the last check.
    let lastElem = allExceptNewest ^. _OldestFirst . to List.last
    when (newerHd ^. prevBlockL . headerHashG /= lastElem) $
        throwGHR $
        sformat ("getHashesRange: newest block parent is not "%
                 "equal to the newest one iterated. It may indicate recent fork or "%
                 "inconsistent request. Newest: "%build%
                 ", last list hash: "%build%", already retrieved (w/o last): "%listJson)
                newerHd
                lastElem
                allExceptNewest

    -- We append last element and convert to nonempty.
    let conv =
           fromMaybe (error "getHashesRange: can't happen") .
           nonEmpty .
           (++ [newer])
    pure $ allExceptNewest & _OldestFirst %~ conv
