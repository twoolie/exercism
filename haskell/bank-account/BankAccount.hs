module BankAccount ( BankAccount, openAccount, closeAccount, getBalance, incrementBalance ) where

import Control.Applicative ((<$>))
import Control.Concurrent (MVar, readMVar, newMVar, modifyMVar, modifyMVar_)

data BankAccount = Acc { _balance :: MVar (Maybe Integer) }

openAccount :: IO BankAccount
openAccount = Acc <$> newMVar (Just 0)

closeAccount :: BankAccount -> IO ()
closeAccount = flip modifyMVar_ (\_ -> return Nothing) . _balance

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance = readMVar . _balance

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance acc i = modifyMVar (_balance acc) incr
  where incr (Just bal) = return $ both $ Just (bal + i)
        incr Nothing    = return $ both $ Nothing
        both x = (x,x)