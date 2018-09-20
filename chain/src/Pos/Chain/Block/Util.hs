module Pos.Chain.Block.Util
       ( checkBodyProof
       ) where

import           Universum

import           Control.Monad.Except (MonadError (..))
import           Formatting (build, sformat, (%))

checkBodyProof
    :: (Buildable bodyProof, Eq bodyProof, MonadError Text m)
    => (body -> bodyProof)
    -> body
    -> bodyProof
    -> m ()
checkBodyProof mkProof body proof = do
    let calculatedProof = mkProof body
    let errMsg =
            sformat ("Incorrect proof of body. "%
                      "Proof in header: "%build%
                      ", calculated proof: "%build)
            proof calculatedProof
    unless (calculatedProof == proof) $ throwError errMsg
