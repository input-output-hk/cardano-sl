----------------------------------------------------------------------------
-- Settings
----------------------------------------------------------------------------

import "hint" HLint.Default
import "hint" HLint.Builtin.All

-- These are just too annoying
ignore "Redundant do"
ignore "Redundant bracket"
ignore "Redundant lambda"
ignore "Redundant $"
ignore "Redundant flip"
ignore "Move brackets to avoid $"

-- Losing variable names can be not-nice
ignore "Eta reduce"
ignore "Avoid lambda"

-- Humans know better
ignore "Use camelCase"
ignore "Use const"
ignore "Use section"
ignore "Use if"
ignore "Use notElem"
ignore "Use fromMaybe"
ignore "Use maybe"
ignore "Use fmap"
ignore "Use foldl"
ignore "Use :"
ignore "Use ++"
ignore "Use ||"
ignore "Use &&"
ignore "Use ?~"
ignore "Use <$>"
ignore "Use ."

-- There's no 'head' in Universum
ignore "Use head"

-- Sometimes [Char] is okay (if it means "a set of characters")
ignore "Use String"

-- We have 'whenJust' for this
ignore "Use Foldable.forM_"

-- Sometimes TemplateHaskell is needed to please stylish-haskell
ignore "Unused LANGUAGE pragma"

-- Some 'data' records will be extended with more fields later,
-- so they shouldn't be replaced with 'newtype' blindly
ignore "Use newtype instead of data"


----------------------------------------------------------------------------
-- Hints with 'id' should use 'identity'
----------------------------------------------------------------------------

warn = any identity ==> or
warn = all identity ==> and
warn = (x >>= identity) ==> join x
warn = (identity =<< x) ==> join x
warn = mapM identity ==> sequence
warn = mapM_ identity ==> sequence_
-- This hint had to be given a name because we ignore "Use fromMaybe" above
-- but we want to keep this one – so we add single quotes here
warn "Use 'fromMaybe'" = maybe x identity ==> fromMaybe x
warn = mapMaybe identity ==> catMaybes
warn = maybe Nothing identity ==> join


----------------------------------------------------------------------------
-- Various stuff
----------------------------------------------------------------------------

warn "Avoid 'both'" = both ==> Control.Lens.each
  where
    note = "If you use 'both' on a 2-tuple and later it's accidentally\n\
    \replaced with a longer tuple, 'both' will be silently applied to only\n\
    \the *last two elements* instead of failing with a type error.\n\
    \  * If you want to traverse all elements of the tuple, use 'each'.\n\
    \  * If 'both' is used on 'Either' here, replace it with 'chosen'."

warn = either (const True) (const False) ==> isLeft
warn = either (const False) (const True) ==> isRight

warn = Data.Map.toAscList (Data.Map.fromList x) ==>
           Universum.sortWith fst x
warn = Data.Map.toDescList (Data.Map.fromList x) ==>
           Universum.sortWith (Down . fst) x

warn = map fst &&& map snd ==> unzip


----------------------------------------------------------------------------
-- Universum
----------------------------------------------------------------------------

warn = Data.Text.pack ==> Universum.toText
warn = Data.Text.unpack ==> Universum.toString

warn = Data.Text.Lazy.pack ==> Universum.toLText
warn = Data.Text.Lazy.unpack ==> Universum.toString

warn = Data.Text.Lazy.toStrict ==> Universum.toText
warn = Data.Text.Lazy.fromStrict ==> Universum.toLText

warn = Data.Text.pack (show x) ==> Universum.show x
warn = Data.Text.Lazy.pack (show x) ==> Universum.show x

warn = Control.Exception.evaluate ==> evaluateWHNF
warn = Control.Exception.evaluate (force x) ==> evaluateNF x
warn = Control.Exception.evaluate (x `deepseq` ()) ==> evaluateNF_ x

warn = void (evaluateWHNF x) ==> evaluateWHNF_ x
warn = void (evaluateNF x)   ==> evaluateNF_ x

suggest = nub ==> Universum.ordNub
  where
    note = "'nub' is O(n^2), 'ordNub' is O(n log n)"

warn = sortBy (comparing f) ==> Universum.sortOn f
  where note = "If the function you are using for 'comparing' is fast \
        \(e.g. 'fst'), use 'sortWith' instead of 'sortOn', because 'sortOn' \
        \caches applications the function and 'sortWith' doesn't."

warn = sortOn fst ==> Universum.sortWith fst
  where note = "'sortWith' will be faster here because it doesn't do caching"
warn = sortOn snd ==> Universum.sortWith snd
  where note = "'sortWith' will be faster here because it doesn't do caching"
warn = sortOn (Down . fst) ==> Universum.sortWith (Down . fst)
  where note = "'sortWith' will be faster here because it doesn't do caching"
warn = sortOn (Down . snd) ==> Universum.sortWith (Down . snd)
  where note = "'sortWith' will be faster here because it doesn't do caching"

warn = fmap concat (mapM f s) ==> Universum.concatMapM f s
warn = concat <$> mapM f s ==> Universum.concatMapM f s

-- Removed for now since we don't want to make people use (some of) our ad-hoc stuff.
-- warn = fmap concat (forM f s) ==> Universum.concatForM s f
-- warn = fmap concat (for f s) ==> Universum.concatForM s f
-- warn = concat <$> forM f s ==> Universum.concatForM s f
-- warn = concat <$> for f s ==> Universum.concatForM s f

suggest = fmap and (sequence s) ==> Universum.andM s
  where note = "Applying this hint would mean that some actions\n\
        \that were being executed previously would no longer be executed."
suggest = and <$> sequence s ==> Universum.andM s
  where note = "Applying this hint would mean that some actions\n\
        \that were being executed previously would no longer be executed."

suggest = fmap or (sequence s) ==> Universum.orM s
  where note = "Applying this hint would mean that some actions\n\
        \that were being executed previously would no longer be executed."
suggest = or <$> sequence s ==> Universum.orM s
  where note = "Applying this hint would mean that some actions\n\
        \that were being executed previously would no longer be executed."

suggest = fmap and (mapM f s) ==> Universum.allM f s
  where note = "Applying this hint would mean that some actions\n\
        \that were being executed previously would no longer be executed."
suggest = and <$> mapM f s ==> Universum.allM f s
  where note = "Applying this hint would mean that some actions\n\
        \that were being executed previously would no longer be executed."

suggest = fmap or (mapM f s) ==> Universum.anyM f s
  where note = "Applying this hint would mean that some actions\n\
        \that were being executed previously would no longer be executed."
suggest = or <$> mapM f s ==> Universum.anyM f s
  where note = "Applying this hint would mean that some actions\n\
        \that were being executed previously would no longer be executed."

-- Unfortunately, these are often bad because they remove a variable name
-- (which usually clarifies things):
--     suggest = (do x <- m; when x a) ==> Universum.whenM m a
--     suggest = (do x <- m; unless x a) ==> Universum.unlessM m a

warn = whenM (not <$> x) ==> unlessM x
warn = unlessM (not <$> x) ==> whenM x

-- Oh boy, we sure have many ways of spelling “pure ()”. Also I checked and
-- HLint isn't smart enough to see reordered case branches.
warn = (case m of Just x -> f x; Nothing -> pure ()  ) ==> Universum.whenJust m f
warn = (case m of Just x -> f x; Nothing -> return ()) ==> Universum.whenJust m f
warn = (case m of Just x -> f x; Nothing -> pass     ) ==> Universum.whenJust m f
warn = (case m of Nothing -> pure ()  ; Just x -> f x) ==> Universum.whenJust m f
warn = (case m of Nothing -> return (); Just x -> f x) ==> Universum.whenJust m f
warn = (case m of Nothing -> pass     ; Just x -> f x) ==> Universum.whenJust m f
warn = (maybe (pure ())   f m) ==> Universum.whenJust m f
warn = (maybe (return ()) f m) ==> Universum.whenJust m f
warn = (maybe pass        f m) ==> Universum.whenJust m f

warn = (m >>= \case Just x -> f x; Nothing -> pure ()  ) ==> Universum.whenJustM m f
warn = (m >>= \case Just x -> f x; Nothing -> return ()) ==> Universum.whenJustM m f
warn = (m >>= \case Just x -> f x; Nothing -> pass     ) ==> Universum.whenJustM m f
warn = (m >>= \case Nothing -> pure ()  ; Just x -> f x) ==> Universum.whenJustM m f
warn = (m >>= \case Nothing -> return (); Just x -> f x) ==> Universum.whenJustM m f
warn = (m >>= \case Nothing -> pass     ; Just x -> f x) ==> Universum.whenJustM m f
warn = (maybe (pure ())   f =<< m) ==> Universum.whenJustM m f
warn = (maybe (return ()) f =<< m) ==> Universum.whenJustM m f
warn = (maybe pass        f =<< m) ==> Universum.whenJustM m f
warn = (m >>= maybe (pure ())   f) ==> Universum.whenJustM m f
warn = (m >>= maybe (return ()) f) ==> Universum.whenJustM m f
warn = (m >>= maybe pass        f) ==> Universum.whenJustM m f

warn = (case m of Just _ -> pure ()  ; Nothing -> x) ==> Universum.whenNothing_ m x
warn = (case m of Just _ -> return (); Nothing -> x) ==> Universum.whenNothing_ m x
warn = (case m of Just _ -> pass     ; Nothing -> x) ==> Universum.whenNothing_ m x
warn = (case m of Nothing -> x; Just _ -> pure ()  ) ==> Universum.whenNothing_ m x
warn = (case m of Nothing -> x; Just _ -> return ()) ==> Universum.whenNothing_ m x
warn = (case m of Nothing -> x; Just _ -> pass     ) ==> Universum.whenNothing_ m x
warn = (maybe x (\_ -> pure ()    ) m) ==> Universum.whenNothing_ m x
warn = (maybe x (\_ -> return ()  ) m) ==> Universum.whenNothing_ m x
warn = (maybe x (\_ -> pass       ) m) ==> Universum.whenNothing_ m x
warn = (maybe x (const (pure ()  )) m) ==> Universum.whenNothing_ m x
warn = (maybe x (const (return ())) m) ==> Universum.whenNothing_ m x
warn = (maybe x (const (pass     )) m) ==> Universum.whenNothing_ m x

warn = (m >>= \case Just _ -> pure ()  ; Nothing -> x) ==> Universum.whenNothingM_ m x
warn = (m >>= \case Just _ -> return (); Nothing -> x) ==> Universum.whenNothingM_ m x
warn = (m >>= \case Just _ -> pass     ; Nothing -> x) ==> Universum.whenNothingM_ m x
warn = (m >>= \case Nothing -> x; Just _ -> pure ()  ) ==> Universum.whenNothingM_ m x
warn = (m >>= \case Nothing -> x; Just _ -> return ()) ==> Universum.whenNothingM_ m x
warn = (m >>= \case Nothing -> x; Just _ -> pass     ) ==> Universum.whenNothingM_ m x
warn = (maybe x (\_ -> pure ()    ) =<< m) ==> Universum.whenNothingM_ m x
warn = (maybe x (\_ -> return ()  ) =<< m) ==> Universum.whenNothingM_ m x
warn = (maybe x (\_ -> pass       ) =<< m) ==> Universum.whenNothingM_ m x
warn = (maybe x (const (pure ()  )) =<< m) ==> Universum.whenNothingM_ m x
warn = (maybe x (const (return ())) =<< m) ==> Universum.whenNothingM_ m x
warn = (maybe x (const (pass     )) =<< m) ==> Universum.whenNothingM_ m x
warn = (m >>= maybe x (\_ -> pure ())    ) ==> Universum.whenNothingM_ m x
warn = (m >>= maybe x (\_ -> return ())  ) ==> Universum.whenNothingM_ m x
warn = (m >>= maybe x (\_ -> pass)       ) ==> Universum.whenNothingM_ m x
warn = (m >>= maybe x (const (pure ())  )) ==> Universum.whenNothingM_ m x
warn = (m >>= maybe x (const (return ()))) ==> Universum.whenNothingM_ m x
warn = (m >>= maybe x (const (pass)     )) ==> Universum.whenNothingM_ m x

warn = (case m of Left x -> f x; Right _ -> pure ()  ) ==> Universum.whenLeft m f
warn = (case m of Left x -> f x; Right _ -> return ()) ==> Universum.whenLeft m f
warn = (case m of Left x -> f x; Right _ -> pass     ) ==> Universum.whenLeft m f
warn = (case m of Right _ -> pure ()  ; Left x -> f x) ==> Universum.whenLeft m f
warn = (case m of Right _ -> return (); Left x -> f x) ==> Universum.whenLeft m f
warn = (case m of Right _ -> pass     ; Left x -> f x) ==> Universum.whenLeft m f
warn = (either f (\_ -> pure ()    ) m) ==> Universum.whenLeft m f
warn = (either f (\_ -> return ()  ) m) ==> Universum.whenLeft m f
warn = (either f (\_ -> pass       ) m) ==> Universum.whenLeft m f
warn = (either f (const (pure ()  )) m) ==> Universum.whenLeft m f
warn = (either f (const (return ())) m) ==> Universum.whenLeft m f
warn = (either f (const (pass     )) m) ==> Universum.whenLeft m f

warn = (m >>= \case Left x -> f x; Right _ -> pure ()  ) ==> Universum.whenLeftM m f
warn = (m >>= \case Left x -> f x; Right _ -> return ()) ==> Universum.whenLeftM m f
warn = (m >>= \case Left x -> f x; Right _ -> pass     ) ==> Universum.whenLeftM m f
warn = (m >>= \case Right _ -> pure ()  ; Left x -> f x) ==> Universum.whenLeftM m f
warn = (m >>= \case Right _ -> return (); Left x -> f x) ==> Universum.whenLeftM m f
warn = (m >>= \case Right _ -> pass     ; Left x -> f x) ==> Universum.whenLeftM m f
warn = (either f (\_ -> pure ()    ) =<< m) ==> Universum.whenLeftM m f
warn = (either f (\_ -> return ()  ) =<< m) ==> Universum.whenLeftM m f
warn = (either f (\_ -> pass       ) =<< m) ==> Universum.whenLeftM m f
warn = (either f (const (pure ()  )) =<< m) ==> Universum.whenLeftM m f
warn = (either f (const (return ())) =<< m) ==> Universum.whenLeftM m f
warn = (either f (const (pass     )) =<< m) ==> Universum.whenLeftM m f
warn = (m >>= either f (\_ -> pure ())    ) ==> Universum.whenLeftM m f
warn = (m >>= either f (\_ -> return ())  ) ==> Universum.whenLeftM m f
warn = (m >>= either f (\_ -> pass)       ) ==> Universum.whenLeftM m f
warn = (m >>= either f (const (pure ())  )) ==> Universum.whenLeftM m f
warn = (m >>= either f (const (return ()))) ==> Universum.whenLeftM m f
warn = (m >>= either f (const (pass)     )) ==> Universum.whenLeftM m f

warn = (case m of Right x -> f x; Left _ -> pure ()  ) ==> Universum.whenRight m f
warn = (case m of Right x -> f x; Left _ -> return ()) ==> Universum.whenRight m f
warn = (case m of Right x -> f x; Left _ -> pass     ) ==> Universum.whenRight m f
warn = (case m of Left _ -> pure ()  ; Right x -> f x) ==> Universum.whenRight m f
warn = (case m of Left _ -> return (); Right x -> f x) ==> Universum.whenRight m f
warn = (case m of Left _ -> pass     ; Right x -> f x) ==> Universum.whenRight m f
warn = (either (\_ -> pure ()    ) f m) ==> Universum.whenRight m f
warn = (either (\_ -> return ()  ) f m) ==> Universum.whenRight m f
warn = (either (\_ -> pass       ) f m) ==> Universum.whenRight m f
warn = (either (const (pure ()  )) f m) ==> Universum.whenRight m f
warn = (either (const (return ())) f m) ==> Universum.whenRight m f
warn = (either (const (pass     )) f m) ==> Universum.whenRight m f

warn = (m >>= \case Right x -> f x; Left _ -> pure ()  ) ==> Universum.whenRightM m f
warn = (m >>= \case Right x -> f x; Left _ -> return ()) ==> Universum.whenRightM m f
warn = (m >>= \case Right x -> f x; Left _ -> pass     ) ==> Universum.whenRightM m f
warn = (m >>= \case Left _ -> pure ()  ; Right x -> f x) ==> Universum.whenRightM m f
warn = (m >>= \case Left _ -> return (); Right x -> f x) ==> Universum.whenRightM m f
warn = (m >>= \case Left _ -> pass     ; Right x -> f x) ==> Universum.whenRightM m f
warn = (either (\_ -> pure ()    ) f =<< m) ==> Universum.whenRightM m f
warn = (either (\_ -> return ()  ) f =<< m) ==> Universum.whenRightM m f
warn = (either (\_ -> pass       ) f =<< m) ==> Universum.whenRightM m f
warn = (either (const (pure ()  )) f =<< m) ==> Universum.whenRightM m f
warn = (either (const (return ())) f =<< m) ==> Universum.whenRightM m f
warn = (either (const (pass     )) f =<< m) ==> Universum.whenRightM m f
warn = (m >>= either (\_ -> pure ())     f) ==> Universum.whenRightM m f
warn = (m >>= either (\_ -> return ())   f) ==> Universum.whenRightM m f
warn = (m >>= either (\_ -> pass)        f) ==> Universum.whenRightM m f
warn = (m >>= either (const (pure ())  ) f) ==> Universum.whenRightM m f
warn = (m >>= either (const (return ())) f) ==> Universum.whenRightM m f
warn = (m >>= either (const (pass)     ) f) ==> Universum.whenRightM m f

warn = mapMaybe leftToMaybe ==> lefts
warn = mapMaybe rightToMaybe ==> rights

warn "Use 'nonEmpty' from Universum" =
    Data.List.NonEmpty.nonEmpty ==> Universum.nonEmpty

warn "Use 'newTVar' from Universum" =
    Control.Concurrent.STM.TVar.newTVar ==> Universum.newTVar
warn "Use 'readTVar' from Universum" =
    Control.Concurrent.STM.TVar.readTVar ==> Universum.readTVar
warn "Use 'writeTVar' from Universum" =
    Control.Concurrent.STM.TVar.writeTVar ==> Universum.writeTVar
warn "Use 'modifyTVar'' from Universum" =
    Control.Concurrent.STM.TVar.modifyTVar' ==> Universum.modifyTVar'
warn "Use 'newTVarIO' from Universum" =
    Control.Concurrent.STM.TVar.newTVarIO ==> Universum.newTVarIO
warn "Use 'readTVarIO' from Universum" =
    Control.Concurrent.STM.TVar.readTVarIO ==> Universum.readTVarIO

warn "Use 'newIORef' from Universum" =
    Data.IORef.newIORef ==> Universum.newIORef
warn "Use 'readIORef' from Universum" =
    Data.IORef.readIORef ==> Universum.readIORef
warn "Use 'writeIORef' from Universum" =
    Data.IORef.writeIORef ==> Universum.writeIORef
warn "Use 'modifyIORef' from Universum" =
    Data.IORef.modifyIORef ==> Universum.modifyIORef
warn "Use 'modifyIORef'' from Universum" =
    Data.IORef.modifyIORef' ==> Universum.modifyIORef'
warn "Use 'atomicModifyIORef' from Universum" =
    Data.IORef.atomicModifyIORef ==> Universum.atomicModifyIORef
warn "Use 'atomicModifyIORef'' from Universum" =
    Data.IORef.atomicModifyIORef' ==> Universum.atomicModifyIORef'
warn "Use 'atomicWriteIORef' from Universum" =
    Data.IORef.atomicWriteIORef ==> Universum.atomicWriteIORef

warn "Use 'lines' from Universum" =
    Data.Text.lines ==> Universum.lines
warn "Use 'unlines' from Universum" =
    Data.Text.unlines ==> Universum.unlines
warn "Use 'words' from Universum" =
    Data.Text.words ==> Universum.words
warn "Use 'unwords' from Universum" =
    Data.Text.unwords ==> Universum.unwords

warn "Use 'fromStrict' from Universum" =
    Data.Text.Lazy.fromStrict ==> Universum.fromStrict
warn "Use 'toStrict' from Universum" =
    Data.Text.Lazy.toStrict ==> Universum.toStrict

warn "Use 'getLine' from Universum" =
    Data.Text.IO.getLine ==> Universum.getLine
warn "Use 'readFile' from Universum" =
    Data.Text.IO.readFile ==> Universum.readFile
warn "Use 'writeFile' from Universum" =
    Data.Text.IO.writeFile ==> Universum.writeFile
warn "Use 'appendFile' from Universum" =
    Data.Text.IO.appendFile ==> Universum.appendFile
warn "Use 'interact' from Universum" =
    Data.Text.Lazy.IO.interact ==> Universum.interact
warn "Use 'getContents' from Universum" =
    Data.Text.Lazy.IO.getContents ==> Universum.getContents

warn "Use '(&&&)' from Universum" =
    (Control.Arrow.&&&) ==> (Universum.&&&)

warn "Use 'MaybeT' from Universum" =
    Control.Monad.Trans.Maybe.MaybeT ==> Universum.MaybeT
warn "Use 'maybeToExceptT' from Universum" =
    Control.Monad.Trans.Maybe.maybeToExceptT ==> Universum.maybeToExceptT
warn "Use 'exceptToMaybeT' from Universum" =
    Control.Monad.Trans.Maybe.exceptToMaybeT ==> Universum.exceptToMaybeT

----------------------------------------------------------------------------
-- Lifted functions in Universum
----------------------------------------------------------------------------

-- concurrency

warn "liftIO is not needed" = liftIO newEmptyMVar ==> Universum.newEmptyMVar
  where
    note = "If you import 'newEmptyMVar' from Universum, it's already lifted"

warn "liftIO is not needed" = liftIO (newMVar x) ==> Universum.newMVar x
  where
    note = "If you import 'newMVar' from Universum, it's already lifted"

warn "liftIO is not needed" = liftIO (putMVar x y) ==> Universum.putMVar x y
  where
    note = "If you import 'putMVar' from Universum, it's already lifted"

warn "liftIO is not needed" = liftIO (readMVar x) ==> Universum.readMVar x
  where
    note = "If you import 'readMVar' from Universum, it's already lifted"

warn "liftIO is not needed" = liftIO (swapMVar x y) ==> Universum.swapMVar x y
  where
    note = "If you import 'swapMVar' from Universum, it's already lifted"

warn "liftIO is not needed" = liftIO (takeMVar x) ==> Universum.takeMVar x
  where
    note = "If you import 'takeMVar' from Universum, it's already lifted"

warn "liftIO is not needed" = liftIO (tryPutMVar x y) ==> Universum.tryPutMVar x y
  where
    note = "If you import 'tryPutMVar' from Universum, it's already lifted"

warn "liftIO is not needed" = liftIO (tryReadMVar x) ==> Universum.tryReadMVar x
  where
    note = "If you import 'tryReadMVar' from Universum, it's already lifted"

warn "liftIO is not needed" = liftIO (tryTakeMVar x) ==> Universum.tryTakeMVar x
  where
    note = "If you import 'tryTakeMVar' from Universum, it's already lifted"

warn "liftIO is not needed" = liftIO (atomically x) ==> Universum.atomically x
  where
    note = "If you import 'atomically' from Universum, it's already lifted"

warn "liftIO is not needed" = liftIO (newTVarIO x) ==> Universum.newTVarIO x
  where
    note = "If you import 'newTVarIO' from Universum, it's already lifted"

warn "liftIO is not needed" = liftIO (readTVarIO x) ==> Universum.readTVarIO x
  where
    note = "If you import 'readTVarIO' from Universum, it's already lifted"

-- IORef

warn "liftIO is not needed" = liftIO (newIORef x) ==> Universum.newIORef x
  where
    note = "If you import 'newIORef' from Universum, it's already lifted"

warn "liftIO is not needed" = liftIO (readIORef x) ==> Universum.readIORef x
  where
    note = "If you import 'readIORef' from Universum, it's already lifted"

warn "liftIO is not needed" = liftIO (writeIORef x y) ==> Universum.writeIORef x y
  where
    note = "If you import 'writeIORef' from Universum, it's already lifted"

warn "liftIO is not needed" = liftIO (modifyIORef x y) ==> Universum.modifyIORef x y
  where
    note = "If you import 'modifyIORef' from Universum, it's already lifted"

warn "liftIO is not needed" = liftIO (modifyIORef' x y) ==> Universum.modifyIORef' x y
  where
    note = "If you import 'modifyIORef'' from Universum, it's already lifted"

warn "liftIO is not needed" = liftIO (atomicModifyIORef x y) ==> Universum.atomicModifyIORef x y
  where
    note = "If you import 'atomicModifyIORef' from Universum, it's already lifted"

warn "liftIO is not needed" = liftIO (atomicModifyIORef' x y) ==> Universum.atomicModifyIORef' x y
  where
    note = "If you import 'atomicModifyIORef'' from Universum, it's already lifted"

warn "liftIO is not needed" = liftIO (atomicWriteIORef x y) ==> Universum.atomicWriteIORef x y
  where
    note = "If you import 'atomicWriteIORef' from Universum, it's already lifted"

-- others

warn "liftIO is not needed" = liftIO Universum.getContents ==> Universum.getContents
  where
    note = "If you import 'getContents' from Universum, it's already lifted"

warn "liftIO is not needed" = liftIO Universum.getLine ==> Universum.getLine
  where
    note = "If you import 'getLine' from Universum, it's already lifted"

warn "liftIO is not needed" = liftIO (Universum.interact x) ==> Universum.interact x
  where
    note = "If you import 'interact' from Universum, it's already lifted"

warn "liftIO is not needed" = liftIO (Universum.interact x) ==> Universum.interact x
  where
    note = "If you import 'interact' from Universum, it's already lifted"

warn "liftIO is not needed" = liftIO (Universum.readFile x) ==> Universum.readFile x
  where
    note = "If you import 'readFile' from Universum, it's already lifted"

warn "liftIO is not needed" = liftIO (Universum.writeFile x y) ==> Universum.writeFile x y
  where
    note = "If you import 'writeFile' from Universum, it's already lifted"

warn "liftIO is not needed" = liftIO (Universum.appendFile x y) ==> Universum.appendFile x y
  where
    note = "If you import 'appendFile' from Universum, it's already lifted"

warn "liftIO is not needed" = liftIO (Universum.openFile x y) ==> Universum.openFile x y
  where
    note = "If you import 'openFile' from Universum, it's already lifted"

warn "liftIO is not needed" = liftIO getArgs ==> Universum.getArgs
  where
    note = "If you import 'getArgs' from Universum, it's already lifted"

warn "liftIO is not needed" = liftIO (exitWith x) ==> Universum.exitWith x
  where
    note = "If you import 'exitWith' from Universum, it's already lifted"

warn "liftIO is not needed" = liftIO exitFailure ==> Universum.exitFailure
  where
    note = "If you import 'exitFailure' from Universum, it's already lifted"

warn "liftIO is not needed" = liftIO exitSuccess ==> Universum.exitSuccess
  where
    note = "If you import 'exitSuccess' from Universum, it's already lifted"

warn "liftIO is not needed" = liftIO (die x) ==> Universum.die x
  where
    note = "If you import 'die' from Universum, it's already lifted"

warn "liftIO is not needed" = liftIO (stToIO x) ==> Universum.stToIO x
  where
    note = "If you import 'stToIO' from Universum, it's already lifted"
