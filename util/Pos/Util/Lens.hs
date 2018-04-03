-- | Utilities related to 'lens' package.

module Pos.Util.Lens
       (
       -- * Custom lenses
         _neHead
       , _neTail
       , _neLast

       -- * Custom LensRules
       , postfixLFields
       , postfixLFields2

       ) where

import           Universum hiding (init, last)
import           Data.List (init, last)

import           Control.Lens (LensRules, lensField, lensRules, mappingNamer)

-- | Lens for the head of 'NonEmpty'.
--
-- We can't use '_head' because it doesn't work for 'NonEmpty':
-- <https://github.com/ekmett/lens/issues/636#issuecomment-213981096>.
-- Even if we could though, it wouldn't be a lens, only a traversal.
_neHead :: Lens' (NonEmpty a) a
_neHead f (x :| xs) = (:| xs) <$> f x

-- | Lens for the tail of 'NonEmpty'.
_neTail :: Lens' (NonEmpty a) [a]
_neTail f (x :| xs) = (x :|) <$> f xs

-- | Lens for the last element of 'NonEmpty'.
_neLast :: Lens' (NonEmpty a) a
_neLast f (x :| []) = (:| []) <$> f x
_neLast f (x :| xs) = (\y -> x :| init xs ++ [y]) <$> f (last xs)

----------------------------------------------------------------------------
-- Custom LensRules
----------------------------------------------------------------------------

postfixLFields :: LensRules
postfixLFields = lensRules & lensField .~ mappingNamer (\s -> [s++"_L"])

postfixLFields2 :: LensRules
postfixLFields2 = lensRules & lensField .~ mappingNamer (\s -> [s++"L"])
