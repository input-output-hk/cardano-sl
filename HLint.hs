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
warn = maybe x identity ==> fromMaybe x
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
  where
    note = "'nonEmpty' is available in Universum, so you don't have to\n\
    \import it from Data.List.NonEmpty"
