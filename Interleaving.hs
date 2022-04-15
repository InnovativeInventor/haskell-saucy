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


-- some simple defs

data Signal = Blocking | Go | Done
data SignalChan = SignalChan {
    c2p :: Chan Signal, -- controller to process
    p2c :: Chan Signal -- process to controller
    }

-- todo: rewrite using SignalChan

{-
mkSignalChan = do
    let sig = SignalChan {c2p = newChan, p2c = newChan}
    return sig
-}

go b = do
    writeChan b Go

done b = do
    writeChan b Done

block c2p p2c = do
    writeChan p2c Blocking

blockUntil sig b = do
    writeChan b Blocking
    blockUntilRec sig b

blockUntilRec sig b = do
    sig <- readChan b
    case sig of 
        sig -> blockUntilRec sig b
        _ -> (return ())

blockUntilNot sig b = do
    writeChan b Blocking
    blockUntilRec sig b

blockUntilNotRec sig b = do
    sig <- readChan b
    case sig of 
        sig -> (return ())
        _ -> blockUntilNotRec sig b

-- some dummy example processes, to demo interleaving
processP c2p p2c c d = do
    blockUntil Go p2c
    writeChan c 1
    result <- readChan d
    liftIO $ putStrLn "seen from P"

processP c2p p2c c d = do
    blockUntil Go p2c 
    readChan c
    writeChan d 1
    liftIO $ putStrLn "seen from Q"


-- total hack, use QuickCheck later
hackyPick :: [a] -> IO a
hackyPick xs = fmap (xs !!) $ randomRIO (0, length xs - 1)

blockUntilAll sig chans = do 
   mapM (blockUntil sig) chans

blockUntilAllNot sig chans = do 
   mapM (blockUntilNot sig) chans

choiceRun fromP toP = do 
   chan <- hackyPick toP -- chan <- pick chans -- QuickCheck?
   writeChan chan Go
   go (c2p chan)

choiceRunBlock fromP toP = do 
   blockUntilAllNot Go fromP
   chan <- hackyPick toP -- chan <- pick chans -- QuickCheck?
   writeChan (c2p chan) Go
   go (c2p chan)


-- Some examples

deadlockEx :: IO ()
deadlockEx = do
  d <- newChan
  c <- newChan

  c2p <- newChan
  p2c <- newChan
  q2c <- newChan
  c2q <- newChan

  -- let sigP = SignalChan {c2p <- newChan, p2c <- newChan}
  -- let sigQ = SignalChan {c2p <- newChan, p2c <- newChan}

  fork $ processP c2p p2c c d
  fork $ processQ c2q q2c c d

  return ()

interleaveEx :: IO ()
interleaveEx = do
  d <- newChan
  c <- newChan

  c2p <- newChan
  p2c <- newChan
  q2c <- newChan
  c2q <- newChan

  fork $ processP c2p p2c c d
  fork $ processQ c2q q2c c d

  -- sigP <- SignalChan {c2p = newChan, p2c = newChan}
  -- sigQ <- SignalChan {c2p = newChan, p2c = newChan}
  -- fork $ processP sigP c d
  -- fork $ processQ sigQ c d

  -- you can choose which ones you want to race to cut down on the combinatorial complexity!
  choiceRun [p2c, q2c] [c2p, c2q]
  choiceRun [p2c, q2c] [c2p, c2q]
  
  return ()
{-

processPWithDone b c d = do
    blockUntil Go b
    writeChan c 1
    result <- readChan d
    liftIO $ putStrLn "seen from P"
    done b

processQWithDone b c d = do
    blockUntil Go b
    readChan c
    writeChan d 1
    liftIO $ putStrLn "seen from Q"
    done b

interleaveExBlockDone :: IO ()
interleaveExBlockDone = do
  d <- newChan
  c <- newChan

  sigP <- SignalChan {c2p = newChan, p2c = newChan}
  sigQ <- SignalChan {c2p = newChan, p2c = newChan}

  fork $ processPWithDone sigP c d
  fork $ processQWithDone sigQ c d

  -- you can choose which ones you want to race to cut down on the combinatorial complexity!
  choiceRunBlock [sigP, sigQ] 
  choiceRunBlock [sigP, sigQ]
  
  return ()

-- scratch

-- let's add some batch operations, as sugar

-- batchChans :: _ -> [(IO (Chan Signal))]
batchChans n = do
    case n of 
        0 -> undefined
        1 -> return [newChan]
        _ -> return newChan:(batchChans (n-1))

pop chans = do
   first <- head chans
   chans <- tail chans
   return first
-}

{-
deadlockExBatchSig :: IO ()
deadlockExBatchSig = do
  d <- newChan
  c <- newChan

  signalChans <- batchChans 2
  first <- head signalChans
  signalChans <- tail signalChans
  second <- head signalChans

  fork $ processP first c d
  fork $ processQ second c d
  return ()
-}
