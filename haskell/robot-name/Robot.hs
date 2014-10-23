module Robot (mkRobot, robotName, resetName) where

import System.Random (randomRIO)
import Control.Monad (replicateM, void)
import Control.Applicative ((<$>))
import Control.Concurrent (MVar, readMVar, swapMVar, newMVar)

data Robot = Robot { _name :: MVar String }

mkRobot :: IO Robot
mkRobot = Robot <$> (genName >>= newMVar)

robotName :: Robot -> IO String
robotName = readMVar . _name

resetName :: Robot -> IO ()
resetName = void . (>>=) genName . swapMVar . _name

genName :: IO String
genName = sequence [rchar,rchar,rnum,rnum,rnum]
	where
		rchar = randomRIO ('A','Z')
		rnum  = randomRIO ('0','9')