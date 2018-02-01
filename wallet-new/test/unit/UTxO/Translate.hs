{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UTxO.Translate (
    -- * Monadic context for the translation from the DSL to Cardano
    Translate
  , runTranslate
  , runTranslateNoErrors
  , withConfig
  , mapTranslateErrors
  , catchTranslateErrors
    -- * Interface to the verifier
  , verify
  , verifyBlocksPrefix
    -- * Convenience re-exports
  , MonadError(..)
  , MonadGState(..)
  ) where

import Universum
import Control.Exception (throw)
import Control.Monad.Except
import Data.Constraint (Dict(..))

import Pos.Block.Error
import Pos.Block.Types
import Pos.Core
import Pos.DB.Class (MonadGState(..))
import Pos.Txp.Toil
import Pos.Update
import Pos.Util.Chrono

import UTxO.Context
import UTxO.Verify (Verify)
import qualified UTxO.Verify as Verify

{-------------------------------------------------------------------------------
  Testing infrastructure from cardano-sl-core

  The genesis block comes from defaultTestConf, which in turn uses
  configuration.yaml. It is specified by a 'GenesisSpec'.
-------------------------------------------------------------------------------}

import Test.Pos.Util (
    withDefConfiguration
  , withDefUpdateConfiguration
  )

{-------------------------------------------------------------------------------
  Translation monad

  The translation provides access to the translation context as well as some
  dictionaries so that we can lift Cardano operations to the 'Translate' monad.
  (Eventually we may wish to do this differently.)
-------------------------------------------------------------------------------}

data TranslateEnv = TranslateEnv {
      teContext :: TransCtxt
    , teConfig  :: Dict HasConfiguration
    , teUpdate  :: Dict HasUpdateConfiguration
    }

newtype Translate e a = Translate {
      unTranslate :: ExceptT e (Reader TranslateEnv) a
    }
  deriving (Functor, Applicative, Monad, MonadError e)

instance MonadReader TransCtxt (Translate e) where
  ask     = Translate $ asks teContext
  local f = Translate . local f' . unTranslate
    where
      f' env = env { teContext = f (teContext env) }

-- | Right now this always returns the genesis policy
instance MonadGState (Translate e) where
  gsAdoptedBVData = withConfig $ return genesisBlockVersionData

-- | Run translation
--
-- NOTE: This uses the default test configuration, and throws any errors as
-- pure exceptions.
runTranslate :: Exception e => Translate e a -> a
runTranslate (Translate ma) =
    withDefConfiguration $
    withDefUpdateConfiguration $
      let env :: TranslateEnv
          env = TranslateEnv {
                    teContext = initContext initCardanoContext
                  , teConfig  = Dict
                  , teUpdate  = Dict
                  }
      in case runReader (runExceptT ma) env of
           Left  e -> throw e
           Right a -> a

-- | Specialised form of 'runTranslate' when there can be no errors
runTranslateNoErrors :: Translate Void a -> a
runTranslateNoErrors = runTranslate

-- | Lift functions that want the configuration as type class constraints
withConfig :: ((HasConfiguration, HasUpdateConfiguration) => Translate e a)
           -> Translate e a
withConfig f = do
    Dict <- Translate $ asks teConfig
    Dict <- Translate $ asks teUpdate
    f

-- | Map errors
mapTranslateErrors :: (e -> e') -> Translate e a -> Translate e' a
mapTranslateErrors f (Translate ma) = Translate $ withExceptT f ma

catchTranslateErrors :: Translate e a -> Translate e' (Either e a)
catchTranslateErrors (Translate (ExceptT (ReaderT ma))) =
    Translate $ ExceptT $ ReaderT $ \env -> fmap Right (ma env)

{-------------------------------------------------------------------------------
  Interface to the verifier
-------------------------------------------------------------------------------}

-- | Run the verifier
verify :: (HasConfiguration => Verify e a) -> Translate e' (Either e (a, Utxo))
verify ma = withConfig $ do
    utxo <- asks (ccUtxo . tcCardano)
    return $ Verify.verify utxo ma

-- | Wrapper around 'UTxO.Verify.verifyBlocksPrefix'
verifyBlocksPrefix
  :: OldestFirst NE Block
  -> Translate e' (Either VerifyBlocksException (OldestFirst NE Undo, Utxo))
verifyBlocksPrefix blocks = do
    CardanoContext{..} <- asks tcCardano
    let tip         = ccHash0
        currentSlot = Nothing
    verify $ Verify.verifyBlocksPrefix
        tip
        currentSlot
        ccLeaders        -- TODO: May not be necessary to pass this if we start from genesis
        (OldestFirst []) -- TODO: LastBlkSlots. Unsure about the required value or its effect
        blocks
