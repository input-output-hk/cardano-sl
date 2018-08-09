module Test.Spec.WalletWorker (
    spec
  ) where

import           Universum

import qualified Data.Text.Buildable
import           Formatting (bprint, shown, (%))
import           Pos.Core.Chrono
import           Test.QuickCheck (arbitrary, frequency, listOf, suchThat)

import           Util.Buildable.Hspec
import           Util.Buildable.QuickCheck

import qualified Cardano.Wallet.Kernel.Actions as Actions

-- declares Arbitrary instance for Text
import           Test.QuickCheck.Instances ()

{-------------------------------------------------------------------------------
  Wallet worker state machine tests
-------------------------------------------------------------------------------}

spec :: Spec
spec = do
    describe "Test wallet worker state machine" $ do

      it "Starts in a valid initial state with no effect on the wallet" $ do
          let StackResult{..} = runStackWorker [] $ Stack [1..10]
          srState `shouldSatisfy` Actions.isValidState
          srState `shouldSatisfy` Actions.isInitialState
          srStack `shouldBe` Stack [1..10]

      it "State invariants are not violated" $ forAll (listOf someAction) $
          \actions -> Actions.isValidState (srState $ runStackWorker actions $ Stack [1..5])

      it "Applies blocks immediately from its initial state" $ do
          let actions = [ Actions.ApplyBlocks (OldestFirst $ 1:|[2,3]) ]
              StackResult{..} = runStackWorker actions $ Stack []
          srStack `shouldBe` Stack [3,2,1]

      it "Applies blocks in the correct order" $ do
          let actions = [ Actions.ApplyBlocks $ OldestFirst $ 1:|[2,3]
                        , Actions.ApplyBlocks $ OldestFirst $ 4:|[5,6] ]
              StackResult{..} = runStackWorker actions $ Stack []
          srStack `shouldBe` Stack [6,5,4,3,2,1]

      it "Can switch to a new fork" $ do
          let actions = [ Actions.ApplyBlocks    $ OldestFirst $ 1:|[2,3]
                        , Actions.RollbackBlocks $ NewestFirst $ 3:|[2]
                        , Actions.ApplyBlocks    $ OldestFirst $ 4:|[5,6] ]
              StackResult{..} = runStackWorker actions $ Stack []
          srState `shouldSatisfy` (not . Actions.hasPendingFork)
          srStack `shouldBe` Stack [6,5,4,1]

      it "Can switch to a new fork by combining actions" $ do
          let actions = [ Actions.ApplyBlocks    $ OldestFirst $ 1:|[2,3]
                        , Actions.RollbackBlocks $ NewestFirst $ 3:|[2]
                        , Actions.ApplyBlocks    $ OldestFirst $ 4:|[]
                      , Actions.ApplyBlocks    $ OldestFirst $ 5:|[6] ]
              StackResult{..} = runStackWorker actions $ Stack []
          srState `shouldSatisfy` (not . Actions.hasPendingFork)
          srStack `shouldBe` Stack [6,5,4,1]

      it "Behaves like the simple stack model, when there is no pending fork" $ do
          let stk0 = Stack [1..100]
              run = (`runStackWorker` stk0)
              doesNotResultInFork = not . Actions.hasPendingFork . srState . run
          forAll (listOf someAction `suchThat` doesNotResultInFork) $
              \actions -> do
                  let StackResult{..} = run actions
                      expectedStack  = execState (mapM actionToStackOp actions) stk0
                  srStack `shouldBe` expectedStack

  where
    runStackWorker :: [Actions.WalletAction Int] -> Stack -> StackResult
    runStackWorker actions stk0 =
        let (s, stk) = runState (Actions.interpList stackOps actions) stk0
        in StackResult { srState = s, srStack = stk }

    -- Bias the actions slightly towards increasing the blockchain size
    someAction :: Gen (Actions.WalletAction Int)
    someAction = frequency [ (10, (Actions.ApplyBlocks . OldestFirst)    <$> arbitrary)
                           , (7,  (Actions.RollbackBlocks . NewestFirst) <$> arbitrary)
                           , (1,   Actions.LogMessage                    <$> arbitrary)
                           ]

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

data StackResult = StackResult
    { srState :: Actions.WalletWorkerState Int
    , srStack :: Stack
    }

stackOps :: Actions.WalletActionInterp (State Stack) Int
stackOps = Actions.WalletActionInterp
    { Actions.applyBlocks  = mapM_ push
    , Actions.switchToFork = \n bs -> do
          replicateM_ n pop
          mapM_ push bs
    , Actions.emit         = const (return ())
    }
  where
    push = interpStackOp . Push
    pop  = interpStackOp Pop

data StackOp = Push Int | Pop
newtype Stack = Stack [Int]
    deriving (Eq, Show)

interpStackOp :: StackOp -> State Stack ()
interpStackOp op = modify $ \stk ->
    case (op, stk) of
        (Push x, Stack xs)     -> Stack (x:xs)
        (Pop,    Stack (_:xs)) -> Stack xs
        (Pop,    Stack [])     -> Stack []

actionToStackOp :: Actions.WalletAction Int -> State Stack ()
actionToStackOp = \case
    Actions.ApplyBlocks    bs -> mapM_ push bs
    Actions.RollbackBlocks bs -> mapM_ (const pop) bs
    Actions.LogMessage _      -> return ()
  where
    push = interpStackOp . Push
    pop  = interpStackOp Pop

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Buildable Stack where
    build (Stack stk) = bprint ("Stack " % shown) stk
