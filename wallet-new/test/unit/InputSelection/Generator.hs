module InputSelection.Generator (
    Event(..)
    -- * Configurable event stream
  , World(..)
  , FromDistrParams(..)
  , fromDistr
  ) where

import           Universum

import           Data.Conduit
import qualified Data.Map.Strict as Map
import qualified Data.Text.Buildable
import           Formatting (bprint, (%))
import           Serokell.Util (listJson, mapJson)
import           Test.QuickCheck

import qualified Cardano.Wallet.Kernel.CoinSelection.Generic as Generic

import           InputSelection.FromGeneric
import           Util.Distr
import           Util.GenHash
import           Util.QuickCheck
import           UTxO.DSL

{-------------------------------------------------------------------------------
  Events
-------------------------------------------------------------------------------}

data Event dom =
    Deposit (Map (Generic.Input dom) (Generic.Output dom))
  | Pay [Generic.Output dom]
  | NextSlot

{-------------------------------------------------------------------------------
  Configurable event stream
-------------------------------------------------------------------------------}

-- | It is well-known that the world divides into us versus them.
data World = Us | Them
  deriving (Eq)

-- | Parameters for 'fromDistr'
data FromDistrParams fDep fPay fNumDep fNumPay =
    ( Distribution fDep
    , Distribution fPay
    , Distribution fNumDep
    , Distribution fNumPay
    ) => FromDistrParams {
      -- | Distribution of deposit values
      fromDistrDep    :: fDep Value

      -- | Distribution of payment values
    , fromDistrPay    :: fPay Value

      -- | Distribution of number of deposits
    , fromDistrNumDep :: fNumDep Int

      -- | Distribution of number of payments
    , fromDistrNumPay :: fNumPay Int

      -- | Number of cycles
    , fromDistrCycles :: Int
    }

-- | Generate event stream using distributions specified in 'FromDistrParams'
fromDistr :: forall m fDep fPay fNumDep fNumPay.
             (LiftQuickCheck m, GenHash m)
          => FromDistrParams fDep fPay fNumDep fNumPay
          -> ConduitT () (Event (DSL GivenHash World)) m ()
fromDistr FromDistrParams{..} = do
    replicateM_ fromDistrCycles $ do
      events <- lift $ do
        numDep <- liftQuickCheck $ drawFromDistr' fromDistrNumDep
        numPay <- liftQuickCheck $ drawFromDistr' fromDistrNumPay
        deps   <- replicateM numDep $ mkDep
        pays   <- replicateM numPay $ mkPay
        liftQuickCheck $ (reverse . (NextSlot :)) <$> shuffle (deps ++ pays)
      mapM_ yield events
  where
    mkDep :: m (Event (DSL GivenHash World))
    mkDep = do
        h <- genHash
        liftQuickCheck $ Deposit . aux h <$> drawFromDistr' fromDistrDep
      where
        aux :: Int   -- hash
            -> Value -- value
            -> Map (Input GivenHash World) (Output GivenHash World)
        aux h val = let i = Input (GivenHash h) 0
                        o = Output Us val
                    in Map.singleton i o

    mkPay :: m (Event (DSL GivenHash World))
    mkPay =
        liftQuickCheck $ Pay . aux <$> drawFromDistr' fromDistrPay
      where
        aux :: Value -> [Output GivenHash World]
        aux val = [Output Them val]

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Buildable World where
  build Us   = "Us"
  build Them = "Them"

instance Generic.CoinSelDom dom => Buildable (Event dom) where
  build (Deposit d) = bprint ("Deposit " % mapJson) d
  build (Pay     p) = bprint ("Pay " % listJson) p
  build NextSlot    = "NextSlot"
