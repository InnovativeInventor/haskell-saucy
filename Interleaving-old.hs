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
data SignalChan = RecordType {
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

block b = do
    writeChan b Blocking

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
processP b c d = do
    blockUntil Go b
    writeChan c 1
    result <- readChan d
    liftIO $ putStrLn "seen from P"

processQ b c d = do
    blockUntil Go b
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

choiceRun chans = do 
   chan <- hackyPick chans -- chan <- pick chans -- QuickCheck?
   writeChan chan Go
   go chan

choiceRunBlock chans = do 
   blockUntilAllNot Go chans
   chan <- hackyPick chans -- chan <- pick chans -- QuickCheck?
   writeChan chan Go
   go chan


-- Some examples

deadlockEx :: IO ()
deadlockEx = do
  d <- newChan
  c <- newChan

  blockingP <- newChan
  blockingQ <- newChan

  fork $ processP blockingP c d
  fork $ processQ blockingQ c d

  return ()

interleaveEx :: IO ()
interleaveEx = do
  d <- newChan
  c <- newChan

  blockingP <- newChan
  blockingQ <- newChan

  fork $ processP blockingP c d
  fork $ processQ blockingQ c d

  -- you can choose which ones you want to race to cut down on the combinatorial complexity!
  choiceRun [blockingP, blockingQ] 
  choiceRun [blockingP, blockingQ]
  
  return ()

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

  blockingP <- newChan
  blockingQ <- newChan

  fork $ processPWithDone blockingP c d
  fork $ processQWithDone blockingQ c d

  -- you can choose which ones you want to race to cut down on the combinatorial complexity!
  choiceRunBlock [blockingP, blockingQ] 
  choiceRunBlock [blockingP, blockingQ]
  
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
