module Main where

import System.Environment (getProgName, getArgs)
import System.Exit        (die)
import Remote.Slave       (startSlave')

main :: IO ()
main = getArgs >>= startSlave

dieWithUsage :: IO a
dieWithUsage = do
  prog <- getProgName
  die $ prog  ++ ": " ++ msg
 where
  msg = "usage: iserv-slave /path/to/storage PORT [-v]"

startSlave :: [String] -> IO ()
startSlave args0
	| "--help" `elem` args0 = dieWithUsage
	| otherwise = do
		(path, port, rest) <- case args0 of
			arg0:arg1:rest -> return (arg0, read arg1, rest)
			_              -> dieWithUsage
		verbose <- case rest of
			["-v"] -> return True
			[]     -> return False
			_      -> dieWithUsage

		startSlave' verbose path port

