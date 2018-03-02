{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}

import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Async
import Control.Exception

data Logic = Logic
  { getTip :: IO Int
  }

data Diffusion = Diffusion
  { announceBlock :: Int -> IO ()
  }

data LogicLayer = LogicLayer
  { logic :: Logic
  , runLogicLayer :: IO ()
  }

data DiffusionLayer = DiffusionLayer
  { diffusion :: Diffusion
  , runDiffusionLayer :: IO ()
  }

withLogicLayer :: ((Diffusion -> IO LogicLayer) -> IO x) -> IO x
withLogicLayer k = bracket acquire release $ \_ -> k $ \diffusion -> do
  tip <- newMVar (0 :: Int)
  let updateTipAndAnnounce = do
        threadDelay 1000000
        j <- modifyMVar tip $ \i -> let !j = i + 1 in pure (j, j)
        announceBlock diffusion j
  pure LogicLayer
    { logic = Logic
        { getTip = readMVar tip
        }
    , runLogicLayer = do
        putStrLn "Running"
        putStrLn "logic"
        putStrLn "layer"
        forever updateTipAndAnnounce
    }
  where
  acquire = putStrLn "Acquiring logic layer."
  release _ = putStrLn "Releasing logic layer."

withDiffusionLayer :: ((Logic -> IO DiffusionLayer) -> IO x) -> IO x
withDiffusionLayer k = bracket acquire release $ \_ -> k $ \logic ->
  pure DiffusionLayer
    { diffusion = Diffusion
        { announceBlock = \i -> do
            putStrLn $ "Announcing block " ++ show i
        }
    , runDiffusionLayer = do
        putStrLn "Running"
        putStrLn "diffusion"
        putStrLn "layer"
        threadDelay 5000000
        t <- getTip logic
        putStrLn $ "Diffusion layer stopping; last observed tip is " ++ show t
        throw (userError "diffusion layer blew up oops")
    }
  where
  acquire = putStrLn "Acquiring diffusion layer."
  release _ = putStrLn "Releasing diffusion layer."

withLayers :: ((LogicLayer, DiffusionLayer) -> IO x) -> IO x
withLayers k =
  withLogicLayer $ \mkLogic ->
    withDiffusionLayer $ \mkDiffusion -> mdo
      logicLayer <- mkLogic (diffusion diffusionLayer)
      diffusionLayer <- mkDiffusion (logic logicLayer)
      k (logicLayer, diffusionLayer)

cslMain :: (forall x . ((LogicLayer, DiffusionLayer) -> IO x) -> IO x) -> IO ()
cslMain layers = layers $ \(logicLayer, diffusionLayer) ->
  void $ concurrently (runLogicLayer logicLayer) (runDiffusionLayer diffusionLayer)

main :: IO ()
main = cslMain withLayers
