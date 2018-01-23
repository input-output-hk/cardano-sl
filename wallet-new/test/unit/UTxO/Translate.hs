{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UTxO.Translate (
    -- * Monadic context for the translation from the DSL to Cardano
    Translate
  , runTranslate
  , withConfig
    -- * Interface to the verifier
  , verify
  , verifyBlocksPrefix
    -- * Convenience re-exports
  , MonadError(..)
  ) where

import Universum hiding (lift)
import Control.Exception (throw)
import Control.Monad.Except (MonadError(..))
import Data.Constraint (Dict(..))
import System.IO.Error (userError)

import Pos.Block.Error
import Pos.Block.Types
import Pos.Core hiding (genesisData, generatedSecrets)
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
      teContext :: Context
    , teConfig  :: Dict HasConfiguration
    , teUpdate  :: Dict HasUpdateConfiguration
    }

newtype Translate a = Translate {
      unTranslate :: ExceptT Text (Reader TranslateEnv) a
    }
  deriving (Functor, Applicative, Monad, MonadError Text)

instance MonadReader Context Translate where
  ask     = Translate $ asks teContext
  local f = Translate . local f' . unTranslate
    where
      f' env = env { teContext = f (teContext env) }

-- | Run translation
--
-- NOTE: This uses the default test configuration, and throws any errors as
-- pure exceptions.
runTranslate :: Translate a -> a
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
           Left  e -> throw (userError (show e))
           Right a -> a

-- | Lift functions that want the configuration as type class constraints
withConfig :: ((HasConfiguration, HasUpdateConfiguration) => Translate a)
           -> Translate a
withConfig f = do
    Dict <- Translate $ asks teConfig
    Dict <- Translate $ asks teUpdate
    f

{-------------------------------------------------------------------------------
  Interface to the verifier
-------------------------------------------------------------------------------}

-- | Run the verifier
verify :: (HasConfiguration => Verify e a) -> Translate (Either e a)
verify ma = withConfig $ do
    utxo <- asks (ccUtxo . tcCardano)
    return $ Verify.verify utxo ma

-- | Wrapper around 'UTxO.Verify.verifyBlocksPrefix'
verifyBlocksPrefix
  :: OldestFirst NE Block
  -> Translate (Either VerifyBlocksException (OldestFirst NE Undo))
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
