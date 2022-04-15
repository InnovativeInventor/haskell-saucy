{-# LANGUAGE TemplateHaskell, ImplicitParams, Rank2Types, LiberalTypeSynonyms, ImpredicativeTypes, RankNTypes, ConstraintKinds, TypeApplications #-}

module Proofs where

import Test.QuickCheck
import StaticCorruptions
import ProcessIO
import Data.List
import System.IO.Unsafe ( unsafePerformIO )
import Control.Concurrent.MonadIO
import Control.Monad (forever, forM_, replicateM_, replicateM)
import Test.QuickCheck.Monadic


-- type TestMonadITM m = (MonadITM m, ?pass :: m Bool)

-- streamContains :: Eq char => IO string -> string -> Bool
streamContains :: Eq char => IO [char] -> [char] -> Bool
streamContains m match = isInfixOf match (unsafePerformIO m) -- uhhh

-- I think this is what success looks like? TODO: ask Prof. Miller
prop_testExec n = streamContains (testParam n) "environment output: 1" 
prop_testExec_fail n = False == (streamContains (testParam n) "not gonna be in the stream")



-- automatically generating advesaries with QuickCheck?
{-
prop_testExecAdv :: MonadAdversary m => Adversary (SttCruptZ2A b d) (SttCruptA2Z a c) a b c d m -> Property 
prop_testExecAdv (adv) = do
  result <- streamContains (runITMinIO 120 $ execUC testEnv idealProtocol dummyFunctionality adv) "env"
  assert (result)
-}



-- setup quickcheck
return []
runTests = $quickCheckAll
