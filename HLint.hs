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
ignore "Use foldl"
ignore "Use :"
ignore "Use ++"
ignore "Use ||"
ignore "Use &&"

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

warn = Data.Map.toAscList (Data.Map.fromList x) ==> Universum.sortOn fst x
warn = Data.Map.toDescList (Data.Map.fromList x) ==> Universum.sortOn (Down . fst) x

warn = forM_ ==> for_

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

suggest = nub ==> Universum.ordNub
  where
    note = "'nub' is O(n^2), 'ordNub' is O(n log n)"

warn = sortBy (comparing f) ==> Universum.sortOn f

warn = fmap concat (mapM f s) ==> Universum.concatMapM f s
warn = concat <$> mapM f s ==> Universum.concatMapM f s

warn = fmap concat (forM f s) ==> Universum.concatForM s f
warn = fmap concat (for f s) ==> Universum.concatForM s f
warn = concat <$> forM f s ==> Universum.concatForM s f
warn = concat <$> for f s ==> Universum.concatForM s f

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

warn = (case m of Just x -> f x; Nothing -> pure ()) ==> Universum.whenJust m f
warn = (case m of Just x -> f x; Nothing -> return ()) ==> Universum.whenJust m f

warn = (case m of Nothing -> pure (); Just x -> f x) ==> Universum.whenJust m f
warn = (case m of Nothing -> return (); Just x -> f x) ==> Universum.whenJust m f

warn = (case m of Left x -> f x; Right _ -> pure ()) ==> Universum.whenLeft m f
warn = (case m of Left x -> f x; Right _ -> return ()) ==> Universum.whenLeft m f

warn = (case m of Left _ -> pure (); Right x -> f x) ==> Universum.whenRight m f
warn = (case m of Left _ -> return (); Right x -> f x) ==> Universum.whenRight m f

warn = mapMaybe leftToMaybe ==> lefts
warn = mapMaybe rightToMaybe ==> rights

warn "Use 'nonEmpty' from Universum" = Data.List.NonEmpty.nonEmpty ==> Universum.nonEmpty

warn "Use 'newTVar' from Universum" = Control.Concurrent.STM.TVar.newTVar ==> Universum.newTVar
warn "Use 'readTVar' from Universum" = Control.Concurrent.STM.TVar.readTVar ==> Universum.readTVar
warn "Use 'writeTVar' from Universum" = Control.Concurrent.STM.TVar.writeTVar ==> Universum.writeTVar
warn "Use 'modifyTVar'' from Universum" = Control.Concurrent.STM.TVar.modifyTVar' ==> Universum.modifyTVar'

warn "Use 'lines' from Universum" = Data.Text.lines ==> Universum.lines
warn "Use 'unlines' from Universum" = Data.Text.unlines ==> Universum.unlines
warn "Use 'words' from Universum" = Data.Text.words ==> Universum.words
warn "Use 'unwords' from Universum" = Data.Text.unwords ==> Universum.unwords

warn "Use 'fromStrict' from Universum" = Data.Text.Lazy.fromStrict ==> Universum.fromStrict
warn "Use 'toStrict' from Universum" = Data.Text.Lazy.toStrict ==> Universum.toStrict

warn "Use 'getLine' from Universum" = Data.Text.IO.getLine ==> Universum.getLine
warn "Use 'readFile' from Universum" = Data.Text.IO.readFile ==> Universum.readFile
warn "Use 'writeFile' from Universum" = Data.Text.IO.writeFile ==> Universum.writeFile
warn "Use 'appendFile' from Universum" = Data.Text.IO.appendFile ==> Universum.appendFile
-- TODO: add interact and getContents (not sure whether strict or lazy) once
-- Universum 0.4 is out


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
