{-# LANGUAGE TemplateHaskell, ImplicitParams, Rank2Types, LiberalTypeSynonyms, ImpredicativeTypes, RankNTypes, ConstraintKinds, TypeApplications, PartialTypeSignatures, FlexibleContexts #-}

module Interleaving where

import Test.QuickCheck
import StaticCorruptions
import ProcessIO
import Data.List
import System.IO.Unsafe ( unsafePerformIO )
import Control.Concurrent.MonadIO
import Control.Monad (forever, forM_, replicateM_, replicateM)
import Test.QuickCheck.Monadic
import System.Random -- TODO: replace with QuickCheck


data Signal = Blocking | Go | Done
data SignalChan = SignalChan {
    to :: Chan Signal, -- controller to process
    from :: Chan Signal -- process to controller
    }

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

-- some dummy example processes, to demo interleaving
processP sig = do
    block sig
    liftIO $ putStrLn "seen from P"
    block sig
    liftIO $ putStrLn "seen from P 2"
    done sig

processQ sig = do
    block sig
    liftIO $ putStrLn "seen from Q"
    block sig
    liftIO $ putStrLn "seen from Q 2"
    done sig
    
deadlockEx :: IO ()
deadlockEx = do
  d <- newChan
  c <- newChan

  c2p <- newChan
  p2c <- newChan
  q2c <- newChan
  c2q <- newChan

  let sigQ = SignalChan {to=c2q, from=q2c}
  let sigP = SignalChan {to=c2p, from=p2c}

  fork $ processP sigP
  fork $ processQ sigQ
  return ()
