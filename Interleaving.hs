{-# LANGUAGE TemplateHaskell, ImplicitParams, Rank2Types, LiberalTypeSynonyms, ImpredicativeTypes, RankNTypes, ConstraintKinds, TypeApplications, PartialTypeSignatures, FlexibleContexts #-}

module Interleaving where

import Test.QuickCheck
import StaticCorruptions
import ProcessIO
import Data.List
import System.IO.Unsafe ( unsafePerformIO )
import Control.Concurrent.MonadIO
import Control.Monad (forever, forM_, replicateM_, replicateM, foldM, filterM)
import Test.QuickCheck.Monadic
import System.Random -- TODO: replace with QuickCheck


data Signal = Blocking | Go | Done
data SignalChan = SignalChan {
    to :: Chan Signal, -- controller to process
    from :: Chan Signal -- process to controller
    }

mkSignalChan :: IO _
mkSignalChan = do
    c2p <- newChan
    p2c <- newChan
    let sig = SignalChan {to=c2p, from=p2c}
    return sig

-- the control sends the go to the process
go sig = do
    writeChan (to sig) Go

-- the process send the Blocking to the controller
block sig = do
    writeChan (from sig) Blocking
    readChan (to sig) -- in the future, check

-- the process send the Done to the controller
done sig = do
    writeChan (from sig) Done

-- blockReady will nicely put the message back after reading
-- if it's not right
-- isReady will return True if Done or Blocking
blockReady sig = do
    signal <- readChan (from sig)
    writeChan (from sig) signal -- write back
    case signal of
        Blocking -> return True
        Done -> return True
        _ -> undefined
        -- _ -> blockReady sig -- this doesn't actually happen, but if it does we should not write back in this case

isDone sig = do
    signal <- readChan (from sig)
    writeChan (from sig) signal -- write back
    case signal of
        Done -> return True
        _ -> return False

notM boolM = do
   bool <- boolM
   case bool of
        True -> return False
	False -> return True
    

foldAnd x y = do
   a <- y
   return (x && a)

blockUntilReady sigs = do
    mapM blockReady sigs

hackyPick :: [a] -> IO a
hackyPick xs = fmap (xs !!) $ randomRIO (0, length xs - 1)

choiceRun sigs = do
   blockUntilReady sigs
   sig <- hackyPick sigs -- chan <- pick chans -- QuickCheck?
   go sig

-- undefined behavior for dur <= 0
fuzz dur sigs = do
   choiceRun sigs
   filterM (notM . isDone) sigs
   case dur of
       1 -> return Done
       _ -> fuzz (dur - 1) sigs

-- some dummy example processes, to demo interleaving
processP sig = do
    block sig
    liftIO $ putStrLn "seen from P at point 1"
    block sig
    liftIO $ putStrLn "seen from P at point 2"
    done sig

processQ sig = do
    block sig
    liftIO $ putStrLn "seen from Q at point 3"
    block sig
    liftIO $ putStrLn "seen from Q  at point 4"
    done sig
    
deadlockEx :: IO ()
deadlockEx = do
  sigQ <- mkSignalChan
  sigP <- mkSignalChan

  fork $ processP sigP
  fork $ processQ sigQ

  return ()

randEx :: IO ()
randEx = do
  sigQ <- mkSignalChan
  sigP <- mkSignalChan

  fork $ processP sigP
  fork $ processQ sigQ

  fuzz 4 [sigQ, sigP] -- duration, signal chans

  return ()
