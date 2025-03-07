{-# LANGUAGE ImplicitParams, ScopedTypeVariables, Rank2Types,
             ConstraintKinds, PartialTypeSignatures
  #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}



module MPC where

import Control.Concurrent.MonadIO
import Data.IORef.MonadIO
import Data.Map.Strict (member, empty, insert, Map, (!))
import qualified Data.Map.Strict as Map
import Control.Monad (forever,foldM)

import Data.Poly
import Data.Field.Galois (Prime, toP)
import Data.Vector (Vector,forM,fromList)

import ProcessIO
import StaticCorruptions
import Polynomial
import Safe

data Void


{--
 We model an MPC protocol as a service that keeps track of a table of
 secret data, but computes a given sequence of operations on this data.

 This is the "Arithmetic Black Box", or `fABB`.

 We have to ensure that all the MPC parties agree on the opcode sequence to run.
 To do this in a flexible way, we let the environment (through a designated
  un-corruptible input party) adaptively choose the opcode sequence, but the
  sequence becomes common knowledge to all the honest parties as well so they
  can follow along.

 Inputs are similarly provided by the adversary.

 The operations available in the fABB service are
  INPUT, LIN, MULT, OPEN.

 - INPUT provides a secret input
 - OPEN discloses a secret value
 - LIN defines a linear combination over existing secret values
 - MULT we'll mention in a moment

 The type of a concrete share ID is Sh, just an integer.
 But polymorphism will play a key role later on: basically the honest protocol
 will treat this as a (forall sh.), while the adversary can see Sh.

 Our main functionality `fMPC` keeps track of not just the secret data,
  but also the entire secret sharing polynomial. Naturally, each of
 the n parties can fetch their own share (and so in total the adversary can fetch t).

 Since our MPC model is designed to demonstrate the compositional style of UC,
  we show off a layered construction MULT, where `fMPC` with MULT present
  is built on top of a simplified `fMPC_sansMult` where it isn't.

 To summarize, our overall construction plan is:
   fMPC_sansMult  ---Beaver---> fMPC ---Adaptor---> fABB

 Part I: We'll start a program for fABB, and the definition of
  fABB using just secrets.

 Part II: Next we'll show the mpcBeaver program, and build up
   the fMPC using secret sharing.

 Part III: The key protocol is substituting mpcBeaver for the MULT
   operation, so we have to construct
          fMPC_sansMult ---Beaver--> fMPC

 Part IV: Completing the proof means giving a simulator simBeaver

 --}

{-- Part I. --}
-- First a test program to illustrate the high-level
-- application interface of fABB.
mpcCircuitTest :: MonadMPCProgram m sh => m Fq
mpcCircuitTest = do
  alice <- input
  bob <- input
  carol <- input

  ab  <- mult alice bob
  abc <- mult ab carol

  result <- openShare abc
  return result

openShare sh = do
  mp <- ?op $ OPEN sh
  let FmpcRes_Poly phi = mp
  return $ eval phi 0

input :: MonadMPCProgram m sh => m sh
input = do
  mp <- ?op INPUT
  let FmpcRes_Sh x = mp
  return x

mult x y = do
  mp <- ?op $ MULT x y
  let FmpcRes_Sh xy = mp
  return xy

data FmpcOp sh = INPUT
               | OPEN sh
               | LIN [(Fq,sh)]
               | CONST Fq
               | MULT sh sh
               | RAND deriving (Eq, Show, Functor)

data FmpcRes sh = FmpcRes_Ok
                | FmpcRes_Fq Fq
                | FmpcRes_Poly PolyFq
                | FmpcRes_Sh sh
                | FmpcRes_Trip (sh,sh,sh) deriving (Eq, Show, Functor)

type MonadMPCProgram m sh = (MonadIO m,
                             ?op :: FmpcOp sh -> m (FmpcRes sh))

{-- Running the application as a protocol using fABB.

  Note that this protocol is parametric (forall sh.) in the type of all
  its channels, both (z2p,p2z) as well as (f2p,p2f).
  This will be important shortly!
--}

data TestAbbZ2P = TestAbbZ2P_Inputs (Fq,Fq,Fq)
                | TestAbbZ2P_Log 

data TestAbbP2Z sh = TestAbbP2Z_Product Fq
                   | TestAbbP2Z_Log [(FmpcOp sh,FmpcRes sh)]

runAbbTestProg :: MonadProtocol m =>
    (forall sh. MonadMPCProgram m sh => m Fq) ->
    Protocol TestAbbZ2P (TestAbbP2Z sh) (FmpcF2P sh) (FmpcP2F sh) m
runAbbTestProg mpcProg (z2p,p2z) (f2p,p2f) = do
   let _op opcode = do
         writeChan p2f $ FmpcP2F_Op opcode
         res <- readChan f2p
         let (FmpcF2P_Op opcode r) = res
         return r

   if ?pid == "InputParty" then do
     mz <- readChan z2p
     let TestAbbZ2P_Inputs (a,b,c) = mz
     writeChan p2f $ FmpcP2F_Input a
     mp <- readChan f2p; let FmpcF2P_Ok = mp
     writeChan p2f $ FmpcP2F_Input b
     mp <- readChan f2p; let FmpcF2P_Ok = mp
     writeChan p2f $ FmpcP2F_Input c
     mp <- readChan f2p; let FmpcF2P_Ok = mp

     x <- let ?op = _op in mpcProg
     writeChan p2z $ TestAbbP2Z_Product x

   else do
    fork $ forever $ do
     mz <- readChan z2p; let TestAbbZ2P_Log = mz
     writeChan p2f $ FmpcP2F_Log
     mp <- readChan f2p; let FmpcF2P_Log log = mp
     writeChan p2z $ TestAbbP2Z_Log log
    return ()
      
   return ()

{-- Defining the fABB functionality (the main body of it comes later)
    Notice that here, unlike in the protocol, the Sh type is concrete,
      just an Int, on all its channels (p2f,f2p) and (a2f,f2a).
  --}
type Sh = Int

fABB :: MonadFunctionality m => Functionality (FmpcP2F Sh) (FmpcF2P Sh) Void Void Void Void m
fABB = let ?n = 0; ?t = 0 in fMPC_ False True



{-- Now for a test environment.
  Notice that here the sh type is parametric in the Z2P/P2Z channels, but
   concrete from the viewpoint of the Z2A/A2Z channels.

, The fABB and fMPC functionalities will represent handles concretely
   as Sh. The functionality itself provides no inherent defense against
   "guessing" handles. In particular, nothing prevents the adversary
   from requesting information about their own shares of intermediate
   values used within a subroutine.

  The parametric Sh type in (p2z,z2p) enforces the "subroutine respecting" property from UC,
   namely that the environment (and therefore also any composed protocols)
   do not access intermediate values (like D,E in beaver multiplication)
   encapsulated by a subroutine.

  Essentially by constraining the type of the environment to be parametric in the handle
   type, the environment cannot ask honest parties to use handles other than those
   returned by the interface.

  A consequence of this polymorphism is that while the logs from honest parties
   contain sh, we can't write code that prints these. Haskellers will appreciate
   the use of `fmap` to make a generic log sanitizer for free.
--}

envTestAbb :: MonadEnvironment m =>
  Environment (TestAbbP2Z sh) (TestAbbZ2P) (SttCruptA2Z (FmpcF2P Sh) Void) (SttCruptZ2A (FmpcP2F Sh) Void) Void Void String m
envTestAbb z2exec (p2z, z2p) (a2z, z2a) (f2z, z2f) pump outp = do
  -- Choose the sid and corruptions
  writeChan z2exec $ SttCrupt_SidCrupt ("mpc","") (Map.fromList [("Observer",())])

  fork $ forever $ do
    (pid,x) <- readChan p2z
    case x of
      TestAbbP2Z_Product fq | pid == "InputParty" -> do
        -- Append result
        liftIO $ putStrLn $ "Z:[" ++ pid ++ "] sent Fq " ++ show fq
      TestAbbP2Z_Log log -> do
        -- Print the log with the handles sanitized...
        --  This is a generic, uses `fmap` from "deriving Functor" FmpcOp
        --  This is necessary to comply with the (forall sh. ...) constraint
        let sanitized :: [(FmpcOp (), FmpcRes ())] =
                 [(fmap (const ()) op, fmap (const ()) res) | (op,res) <- log]
        liftIO $ putStrLn $ "Z:[" ++ pid ++ "] sent Log " ++ show sanitized
    ?pass

  -- Listen to log from dummy adversary
  fork $ forever $ do
    mf <- readChan a2z
    case mf of
      SttCruptA2Z_P2A (pid, FmpcF2P_Log log) | pid == "Observer" -> do
           -- Log received by a corrupt party (we can print the log)
           liftIO $ putStrLn $ "Z: Observer receive Log: " ++ show log
           ?pass

  () <- readChan pump; liftIO $ putStrLn "pump"
  writeChan z2p ("InputParty", TestAbbZ2P_Inputs (2,3,4))

  () <- readChan pump; liftIO $ putStrLn "pump"
  writeChan z2a $ SttCruptZ2A_A2P ("Observer", FmpcP2F_Log)

  () <- readChan pump; liftIO $ putStrLn "pump"
  writeChan z2p $ ("ObserverHon", TestAbbZ2P_Log)

  () <- readChan pump
  writeChan outp "environment output: 1"

testMpc0 = runITMinIO 120 $ execUC envTestAbb (runAbbTestProg mpcCircuitTest) (fABB) dummyAdversary


{--
   Now we proceed with filling out the missing definitions of fABB and fMPC.
   The definition is based around two parts,
    I.(A) a handler for every opcode,
    I.(B) a generic shell, that keeps track of the log of all the operations
      and their results, which it can serve to any party upon request.

   The generic shell (B) has a flag to pick either the idealized ABB (for now)
    or more concrete MPC handlers (for later). So here its type is general,
    storing a PolyFq when really just an Fq would do (since for Abb it's
    always just degree-0).

   Remember that the functionality will define a concrete Sh handle type,
    but this will only be important in part (B). For the opcodes (A) these
    treat the handles opaquely too, but this is just for convenience.
 --}

-- I.(A) Opcode handlers
data FmpcP2F sh = FmpcP2F_Op (FmpcOp sh)
                | FmpcP2F_Log
                | FmpcP2F_Input Fq
                | FmpcP2F_MyShare sh deriving (Show, Functor)

data FmpcF2P sh = FmpcF2P_Op (FmpcOp sh) (FmpcRes sh)
                | FmpcF2P_Log [(FmpcOp sh,FmpcRes sh)]
                | FmpcF2P_Ok
                | FmpcF2P_WrongFollow
                | FmpcF2P_MyShare Fq deriving (Show, Functor)

doAbbOp :: (MonadIO m) => (sh -> m Fq) -> (Fq -> m sh) -> IORef [Fq] -> FmpcOp sh -> m (FmpcRes sh)
doAbbOp readSecret storeFresh inputs op = do
     case op of
       MULT x y -> do
         -- Create a new entry by multiplying two existing ones
         x <- readSecret x
         y <- readSecret y
         xy <- storeFresh (x*y)
         return $ FmpcRes_Sh xy

       LIN cs -> do
         -- Create a new entry from a linear combination of existing ones
         r <- foldM (\r -> \(c::Fq,sh) -> do
            x <- readSecret sh
            return $ r + c * x) 0 cs
         k <- storeFresh r
         return $ FmpcRes_Sh k

       OPEN sh -> do
         -- Publish this value in the log
         x <- readSecret sh
         return $ FmpcRes_Poly (polyFromCoeffs [x])

       INPUT -> do
         -- Collect next input provided by the input party
         inps <- readIORef inputs
         let x:rest = inps
         writeIORef inputs rest
         k <- storeFresh x
         return $ FmpcRes_Sh k

-- (I).B. Generic log handler for fMPC
{--
 Here when defining the lag handler shell
--}
fMPC_ :: MonadMPC_F m => Bool -> Bool -> Functionality (FmpcP2F Sh) (FmpcF2P Sh) Void Void Void Void m
fMPC_ hasMPC hasMult (p2f, f2p) (_,_) (_,_) = do

  -- Log of operations and results
  ops    <- newIORef ([] :: [(FmpcOp Sh, FmpcRes Sh)])

  -- Inputs provided by input party
  inputs <- newIORef []

  -- Maps share IDs to secrets
  -- In MPC mode, these will be degree-t polys, in
  -- ABB mode they will only be constant (degree-0)
  shareTbl <- newIORef (Map.empty :: Map Sh PolyFq)

  -- Generate a fresh handle
  freshCtr <- newIORef 0
  let fresh = do
        x <- readIORef freshCtr
        modifyIORef freshCtr $ (+) 1
        return x

  -- Counters viewed by each of the participant parties
  let initCtrs = [("P:"++show i, 0) | i <- [1.. ?n]]
  counters <- newIORef $ Map.fromList initCtrs

  -- Commit this operation and output to the log
  let commit op outp = do
        modifyIORef ops $ (++ [(op,outp)])
        writeChan f2p $ ("InputParty", FmpcF2P_Op op outp)

  fork $ forever $ do
   (pid,m) <- readChan p2f
   case m of
    -- Anyone can see the log
    FmpcP2F_Log -> do
       log <- readIORef ops
       writeChan f2p (pid, FmpcF2P_Log log)

    -- Only the input party can provide inputs
    FmpcP2F_Input x | pid == "InputParty" -> do
       modifyIORef inputs $ (++ [x])
       writeChan f2p (pid, FmpcF2P_Ok)

    -- Operations send from the input party get carried out
    -- immediately and committed to the log of operations.
    FmpcP2F_Op op | pid == "InputParty" -> do
     if hasMPC then do
       -- In the MPC mode, let the program read/storeFresh the
       -- table of polynomial secret sharings
       let storeFresh phi = do {
           sh <- fresh
           modifyIORef shareTbl $ Map.insert sh phi
           return sh }
       let readSharing sh = readIORef shareTbl >>= return . (! sh)
       res <- doMpcOp hasMult readSharing storeFresh inputs op
       commit op res
     else do
       -- In the ABB mode, we'll only store constant polynomials.
       let storeFresh x = do {
           sh <- fresh
           modifyIORef shareTbl $ Map.insert sh (polyFromCoeffs [x])
           return sh }
       let readSecret sh = do {
           phi <- readIORef shareTbl >>= return . (! sh)
           return (eval phi 0) }
       res <- doAbbOp readSecret storeFresh inputs op

       commit op res
     
    -- Operations from MPC parties are in "Follow" mode.
    -- They have to provide a next op, which is matched up
    -- against the next one chosen by the input party.
    -- They receive the output, typically a handle.
    FmpcP2F_Op op | hasMPC && (not (pid == "InputParty")) -> do
     c <- readIORef counters >>= return . (! pid)
     oplist <- readIORef ops
     let (op',res) = oplist !! c
     if op == op' then do
       modifyIORef counters $ Map.insert pid (c+1)
       writeChan f2p $ (pid, FmpcF2P_Op op res)
     else
       writeChan f2p $ (pid, FmpcF2P_WrongFollow)

    FmpcP2F_MyShare sh | hasMPC -> do
     -- Parse from pid
     let i = case pid of "P:1" -> 1
                         "P:2" -> 2
                         _ -> error "MyShare called by someone else"
     tbl <- readIORef shareTbl
     let mf = Map.lookup sh tbl
     case mf of
        Just phi -> do
           let x = eval phi i
           writeChan f2p $ (pid, FmpcF2P_MyShare x)
        Nothing -> do
           writeChan f2p $ (pid, FmpcF2P_WrongFollow)
    _ -> do
     error "unmatched operation"

  return ()
         

{-- Part II. --}  
{----
Before we define the fMPC hybrid world functionality in more detail,
  we want to motivate some of our design choices, especially being
  pseudocode-friendly.

 As our starting point, we know we want to write MPC pseudocode that
 looks like:

 BeaverMult: [x] [y]
   Preprocessing get [a][b][ab].
   D = open([x]-[a])
   E = open([y]-[b])
   return [xy] := DE + [ab] + D[b] + E[a]

 Behold!
 ---}

mpcBeaver :: MonadMPCProgram m sh => sh -> sh -> m sh
mpcBeaver x y = do
  (a,b,ab) <- getTriple
  d <- openShare =<< sub x a 
  e <- openShare =<< sub y b
  de <- constant (d*e)
  xy <- lin [(1,de),(1,ab),(d,b),(e,a)]
  return xy

-- These (lin, constant, etc.) are some simple wrappers over
-- the ?op interface that this MonadMPCProgram provides for talking
-- to the functionality
lin xs = do
  rsp <- ?op $ LIN xs
  let FmpcRes_Sh r = rsp
  return r

constant v = do
  rsp <- ?op $ CONST v
  let FmpcRes_Sh r = rsp
  return r

sub x a = lin [(1,x),(-1,a)]

getTriple :: MonadMPCProgram m sh => m (sh,sh,sh)
getTriple = do
  r <- ?op $ RAND
  let FmpcRes_Trip(a,b,ab) = r
  return (a,b,ab)

{--
 Now we get to the fMPC definition. Since we reuse the generic shell
 from earlier, all we have to do now is give new opcode handlers.
--}

type MonadMPC_F m = (MonadFunctionality m,
                     ?n :: Int,
                     ?t :: Int)

fMPC :: MonadMPC_F m => Functionality (FmpcP2F Sh) (FmpcF2P Sh) Void Void Void Void m
fMPC = fMPC_ True True

fMPC_sansMult :: MonadMPC_F m => Functionality (FmpcP2F Sh) (FmpcF2P Sh) Void Void Void Void m
fMPC_sansMult = fMPC_ True False


doMpcOp :: (MonadMPC_F m) => Bool -> (sh -> m PolyFq)  -> (PolyFq -> m sh) -> IORef [Fq] -> FmpcOp sh -> m (FmpcRes sh)
doMpcOp hasMult readSharing storeFresh inputs op = do
   case op of
       MULT x y -> do
         -- This is a parameter so we can show how to realize it from
         -- the other operations. Generates a random polynomial whose
         -- zero-value coincides with the product of x and y
         if hasMult then do
           xphi <- readSharing x
           yphi <- readSharing y
           phi <- randomWithZero ?t (eval xphi 0 * eval yphi 0)
           liftIO $ putStrLn $ "PHI" ++ show phi
           xy <- storeFresh phi
           return $ FmpcRes_Sh xy
         else error "mult unimplemented"

       LIN cs -> do
         -- Construct a new secret sharing polynomial simply by
         -- scaling and summing the linear combination of existing
         -- polys from the table
         r <- foldM (\r -> \(c,sh) ->  do
                     x <- readSharing sh
                     return $ r + (polyFromCoeffs [c]) * x) polyZero cs
         k <- storeFresh r
         return $ FmpcRes_Sh k

       OPEN k -> do
         -- The result of opening is included directly in the log
         phi <- readSharing k
         return $ FmpcRes_Poly phi

       CONST v-> do
         -- Create the constant (degree-0) poly
         let phi = polyFromCoeffs [v]
         k <- storeFresh phi
         return $ FmpcRes_Sh k

       RAND -> do
         -- Return a beaver triple
         a <- randomDegree ?t
         b <- randomDegree ?t
         ab <- randomWithZero ?t (eval a 0 * eval b 0)
         ka <- storeFresh a
         kb <- storeFresh b
         kab <- storeFresh ab
         return $ FmpcRes_Trip (ka, kb, kab)

       INPUT -> do
         -- Collect inputs provied by the input party
         inps <- readIORef inputs
         let x:rest = inps
         writeIORef inputs rest
         phi <- randomWithZero ?t x
         k <- storeFresh phi
         return $ FmpcRes_Sh k
  

{-- Part III. --}
{--
 Recall that our goal is to do the construction:
    fMPC_sansMult ---mpcBeaver--> fMPC

 So it's not enough just to run mpcBeaver, we also need an adaptor
   around it so it presents on its (z2p,p2z) channels as fMPC,
   even though it actually interacts with the fMPC_sansMult.
   Basically it needs to substitute the MULT operations with the
   mpcBeaver program, while passing everything else through.
--}

-- This is the adaptor code that replaces the MULT operation
-- from the ideal fMPC functionality with the mpcBeaver subroutine
runMPCnewmul :: MonadProtocol m =>
    (forall sh. MonadMPCProgram m sh => sh -> sh -> m sh) ->
    Protocol (FmpcP2F sh) (FmpcF2P sh) (FmpcF2P sh) (FmpcP2F sh) m
runMPCnewmul mulProg (z2p,p2z) (f2p,p2f) = do

   let _op opcode = do
         writeChan p2f $ FmpcP2F_Op opcode
         res <- readChan f2p
         let (FmpcF2P_Op opcode r) = res
         return r

   log <- newIORef []
   let commit opcode res = do {
       modifyIORef log $ (++ [(opcode,res)])
       writeChan p2z $ FmpcF2P_Op opcode res

   fork $ forever $ do
        zp <- readChan z2p
        case zp of
          FmpcP2F_Op (MULT x y) -> do
             -- Intercept the "MULT" operations and replace them with
             -- a call to the MPC program
             xy <- let ?op = _op in mulProg x y
             commit (MULT x y) (FmpcRes_Sh xy)
             
          FmpcP2F_Op op -> do
             -- Everything else, we can simply forward along and 
             -- expect one response
             writeChan p2f $ zp
             mr <- readChan f2p
             let FmpcF2P_Op op r = mr
             commit op r

          FmpcP2F_Log -> do
             -- When exposing the log, we want to present our local log
             -- (e.g., including the MULT) instead of the one from fMPC
             l <- readIORef log
             writeChan p2z $ FmpcF2P_Log l

          FmpcP2F_MyShare sh -> do
             -- Forward request for Shares in-tact
             writeChan p2f zp
             readChan f2p >>= writeChan p2z
          
   return ()



--- This test environment should give a good coverage of all the interesting real-world protocol behaviors.
envTestMPC :: MonadEnvironment m =>
  Environment              (FmpcF2P sh)                    (FmpcP2F sh)
              (SttCruptA2Z (FmpcF2P Sh) Void) (SttCruptZ2A (FmpcP2F Sh) Void)
              Void Void String m
envTestMPC z2exec (p2z, z2p) (a2z, z2a) (f2z, z2f) pump outp = do
   -- Choose the sid and corruptions
  writeChan z2exec $ SttCrupt_SidCrupt ("mpc","") (Map.fromList [("Observer",()),("P:1",())])

  -- Opened Values
  openTable <- newIORef $ Map.fromList [("P:"++show i, []) | i <- [1.. 3]]
  lastHandle <- newIORef Nothing
  lastIntHandle <- newIORef Nothing

  let sendInput ipp2f = do
        writeChan z2p $ ("InputParty", ipp2f)
   
  fork $ forever $ do
    (pid,m) <- readChan p2z
    -- Store the opaque handles received from honest parties
    case m of FmpcF2P_Op op (FmpcRes_Sh sh) -> writeIORef lastHandle (Just sh)
              _ -> return ()
    let sanitized = fmap (const ()) m
    printEnvIdeal $ "Z:[" ++ pid ++ "] sent " ++ show sanitized
    ?pass

  fork $ forever $ do
    mf <- readChan a2z
    case mf of
      SttCruptA2Z_P2A (pid, m) -> do
        -- Store the concrete handles received from corrupt party
        case m of
          FmpcF2P_Op op (FmpcRes_Sh sh) -> writeIORef lastIntHandle (Just sh)
          FmpcF2P_Log log | pid == "Observer" -> do
            printEnvReal $ "[" ++pid++ "] (corrupt) received log: "
            forM (fromList log) $ printEnvReal . show

            -- Check the equation is satisfied
            
            
            return ()
          _ -> do
            liftIO $ putStrLn $ "Z: [" ++pid++ "] (corrupt) received: " ++ show m
        ?pass

  -- Send inputs through honest InputParty
  () <- readChan pump; liftIO $ putStrLn "pump"
  sendInput $ (FmpcP2F_Op $ CONST 2)
  () <- readChan pump; liftIO $ putStrLn "pump"
  _x <- readIORef lastHandle; let Just x = _x
  sendInput $ (FmpcP2F_Op $ CONST 5)
  () <- readChan pump; liftIO $ putStrLn "pump"
  _y <- readIORef lastHandle; let Just y = _y
  sendInput $ (FmpcP2F_Op $ MULT x y)
  () <- readChan pump; liftIO $ putStrLn "pump"
  _xy <- readIORef lastHandle; let Just xy = _xy
  sendInput $ (FmpcP2F_Op $ OPEN xy)

  -- Follow from honest party
  () <- readChan pump; liftIO $ putStrLn "pump"
  writeChan z2p $ ("P:2", FmpcP2F_Op $ CONST 2)
  () <- readChan pump; liftIO $ putStrLn "pump"
  _x <- readIORef lastHandle; let Just x = _x
  writeChan z2p $ ("P:2", FmpcP2F_Op $ CONST 5)
  () <- readChan pump; liftIO $ putStrLn "pump"
  _y <- readIORef lastHandle; let Just y = _y
  liftIO $ putStrLn "WrongFollow in Ideal, FmpcRes_Trip in real"
  writeChan z2p $ ("P:2", FmpcP2F_Op $ RAND)
  -- writeChan z2p $ ("P:2", FmpcP2F_Op $ MULT x y)
  -- () <- readChan pump; liftIO $ putStrLn "pump"
  --_xy <- readIORef lastHandle; let Just xy = _xy
  -- writeChan z2p $ ("P:2", FmpcP2F_MyShare xy)
  
  -- Logs from Observer (a corrupt party)
  () <- readChan pump; liftIO $ putStrLn "pump"
  writeChan z2a $ SttCruptZ2A_A2P ("Observer", FmpcP2F_Log)

  -- My Share from one of the corrupt parties
  () <- readChan pump; liftIO $ putStrLn "pump"
  writeChan z2a $ SttCruptZ2A_A2P ("P:1", FmpcP2F_MyShare 8)

  -- Follow along (from a corrupt party)
  () <- readChan pump; liftIO $ putStrLn "pump"
  writeChan z2a $ SttCruptZ2A_A2P ("P:1", FmpcP2F_Op $ CONST 2)
  () <- readChan pump; liftIO $ putStrLn "pump"
  _x <- readIORef lastIntHandle; let Just x = _x  
  writeChan z2a $ SttCruptZ2A_A2P ("P:1", FmpcP2F_Op $ CONST 5)
  () <- readChan pump; liftIO $ putStrLn "pump"
  -- Given the dummy adversary in the Ideal world, this matches
  _y <- readIORef lastIntHandle; let Just y = _y  
  writeChan z2a $ SttCruptZ2A_A2P ("P:1", FmpcP2F_Op $ MULT x y)
  -- Given the dummy adversary in the Real world, this matches
  () <- readChan pump; liftIO $ putStrLn "pump"
  writeChan z2a $ SttCruptZ2A_A2P ("P:1", FmpcP2F_Op $ RAND)

  -- Logs from an honest party
  () <- readChan pump; liftIO $ putStrLn "pump"  
  sendInput $ FmpcP2F_Log
  
  () <- readChan pump
  writeChan outp "environment output: 1"

runMPCFunc :: MonadFunctionality m => Int -> Int ->
    (MonadMPC_F m => Functionality a b c d e f m) ->
     Functionality a b c d e f m
runMPCFunc n t f = let ?n = n; ?t = t in f

testMpc1Ideal = runITMinIO 120 $ execUC envTestMPC (idealProtocol) (runMPCFunc 3 1 $ fMPC) dummyAdversary

testMpc1Real = runITMinIO 120 $ execUC envTestMPC (runMPCnewmul mpcBeaver) (runMPCFunc 3 1 $ fMPC_sansMult) dummyAdversary

{-- Part IV. --}
-- Now it's time to complete our simulator-based security proof for the construction
--        fMPC_sansMult --mpcBeaver--> fMPC
--
-- We need to construct a simulator simBeaver for the dummyAdversary such that 
-- forall Z. execUC Z (mpcBeaver)    (fMPC_sansMult) (dummyAdversary)
--         ~ execUC Z (idealProtocol)(fMPC)          (simBeaver)
--
-- In a nutshell, the runs an internal mirror of fMPC, with the "shareTble"
--  and all. As many operations as possible are passed on to the ideal world,
--  but some are separated. You can skip to the `MULT` case of the commit subroutine
--  for the application-specific part.
--
-- Handles from the environment (over z2a/a2z channels) are kept separate from handles
--  exchanged with the ideal world (over p2a/a2p channels).
--  The (z2a/a2z) interactions have concrete type Sh, while the ideal world
--  interactions have a *mostly* opaque type, (Ord sh). This allows the
--  environment to store handles in the map, but it cannot generate handles
--  other than what it has received.
--
-- This is the key way in which the typechecker pays off in helping structure
--  the simulator.
--
--  
--
-- 
-- 

simBeaver :: (Ord sh, MonadAdversary m) => Adversary (SttCruptZ2A (FmpcP2F Sh) Void) (SttCruptA2Z (FmpcF2P Sh) Void) (FmpcF2P sh) (FmpcP2F sh) Void Void m
simBeaver (z2a, a2z) (p2a, a2p) (_, _) = do
  let n = 3; t=1

  -- Simulate the real world fMPC the corrupt parties would interact with
  let initCtrs = [("P:"++show i, 0) | i <- [1.. n]]
  counters <- newIORef $ Map.fromList initCtrs
  a2zlog <- newIORef []  
  let addLog (op, res) = modifyIORef a2zlog $ (++ [(op,res)])
  
  -- Store the shares other than the pass-through ones
  shareTable <- newIORef (Map.empty :: Map Sh PolyFq)

  -- Mapping from sh the fMPC in the ideal world to
  -- Sh in the environment's real interaction with Sim.
  r2iTable <- newIORef (Map.empty :: Map sh Sh)
  i2rTable <- newIORef (Map.empty :: Map Sh sh)

  -- Generate a fresh handle
  freshCtr <- newIORef 0
  let fresh = do
        x <- readIORef freshCtr
        modifyIORef freshCtr $ (+) 1
        return x

  -- Generate a fresh handle, but it's mapped to a share
  --   in the Ideal World functionality
  let freshFromIdeal sh = do
        sSh <- fresh
        modifyIORef i2rTable $ Map.insert sSh sh
        modifyIORef r2iTable $ Map.insert sh sSh
        return sSh

  -- Generate a fresh handle, but it's mapped to a share
  --   in the provided polynomial.
  let freshSimShare phi = do
        sSh <- fresh
        modifyIORef shareTable $ Map.insert sSh phi
        return sSh

  -- Fetch share of corrupted party
  let myShare pid sh = do
        -- Fetch from the Ideal functionality if allowed, else access the
        -- local value
        tbl <- readIORef i2rTable
        let i = case pid of "P:1" -> 1
                            "P:2" -> 2
                            _ -> error "MyShare called by someone else"
        case Map.lookup sh tbl of
          Just s -> do
            writeChan a2p (pid, FmpcP2F_MyShare s)
            mf <- readChan p2a
            let (pid, FmpcF2P_MyShare x) = mf
            return x
          Nothing -> do
            phi <- readIORef shareTable >>= return .(! sh)
            return $ eval phi i
  
  -- We'll keep track of a log of simulated real-world operations. This will be
  -- populated by following along with the Ideal world operations. Every time
  -- the log reflects a new operation, the Ideal world operation will be
  -- committed here, processing one operation at a time.
  -- The interesting case is when MULT shows up in the log, since we need to
  -- substitute BeaverMul operations for it.
  
  let commit opres = do{
      let (op, res) = opres 
      -- liftIO $ putStrLn $ "Commit" ++ show (fmap (const ()) op, fmap (const ()) res)
      case opres of
        (MULT x y, FmpcRes_Sh xy) -> do
           -- TODO: This is an inadequate simulation! Too many degrees of freedom
           -- hint: solve for ab rather than sample
           -- TODO: need to use functionality to look up our shares for x, y
          
           -- Step 1. Create simulated polynomials for all the intermediate values
           -- and preprocessings that show up in the real world protocol but
           -- not the ideal world.
           a  <- freshSimShare =<< randomDegree t
           b  <- freshSimShare =<< randomDegree t
           ab <- freshSimShare =<< randomDegree t           
           dphi <- randomDegree t
           ephi <- randomDegree t
           let d :: Fq = eval dphi 0
           let e :: Fq = eval ephi 0
           x_a <- freshSimShare dphi
           y_b <- freshSimShare ephi
           de <- freshSimShare (polyFromCoeffs [d*e])
           _xy <- freshFromIdeal xy

           -- Step 2. Create a simulated real world log. The Ideal world has a
           --  MULT, so the simulated real will have the BeaverMul ops.
           tbl <- readIORef r2iTable
           addLog (RAND, FmpcRes_Trip (a, b, ab))
           addLog (LIN [(1, tbl ! x),(-1,a)], FmpcRes_Sh x_a)
           addLog (OPEN x_a, FmpcRes_Poly dphi)
           addLog (LIN [(1, tbl ! y),(-1,b)], FmpcRes_Sh y_b)
           addLog (OPEN y_b, FmpcRes_Poly ephi)
           addLog (CONST (d*e), FmpcRes_Sh de)
           addLog (LIN [(1,de),(1,ab),(d,b),(e,a)], FmpcRes_Sh _xy) --}
           return ()

        (op, FmpcRes_Sh xy) -> do
           freshFromIdeal xy
           tbl <- readIORef r2iTable
           addLog (fmap (tbl !) op,
                   fmap (tbl !) res)
           return ()

        (op, FmpcRes_Trip (a, b, ab)) -> do
           freshFromIdeal a; freshFromIdeal b; freshFromIdeal ab
           tbl <- readIORef r2iTable
           addLog (fmap (tbl !) op,
                   fmap (tbl !) res)
           return ()
        (op,res) -> do
           tbl <- readIORef r2iTable
           addLog (fmap (tbl !) op,
                   fmap (tbl !) res)
           return ()
      return ()

  -- Only Process the ideal world log entries that are new, since
  -- the last time we accessed it.
  logCtr <- newIORef 0  
  let syncLog pid = do
      -- Fetch the current log
      writeChan a2p $ (pid, FmpcP2F_Log)
      mf <- readChan p2a
      let (pid, FmpcF2P_Log log) = mf
      -- liftIO $ putStrLn $ "simBeaver: Received log" ++ show (fmap (const ()) mf)
      -- Commit just the new entries
      t <- readIORef logCtr
      let tail = drop t log
      modifyIORef logCtr (+ length tail)
      forM (fromList tail) $ commit

  fork $ forever $ do
    mf <- readChan z2a
    let SttCruptZ2A_A2P (pid, m) = mf
    -- liftIO $ putStrLn $ "sim: z2a a2p s " ++ show (fmap (const ()) m)
    case m of
      FmpcP2F_Log -> do
        syncLog pid
        log <- readIORef a2zlog
        writeChan a2z $ SttCruptA2Z_P2A (pid, FmpcF2P_Log log)
        return ()
        
      FmpcP2F_Op op -> do
        -- In real would match with log
        syncLog pid
        c <- readIORef counters >>= return . (! pid)
        oplist <- readIORef a2zlog
        let (op',res) = oplist !! c
        if op == op' then do
          modifyIORef counters $ Map.insert pid (c+1)
          writeChan a2z $ SttCruptA2Z_P2A $ (pid, FmpcF2P_Op op res)
        else
          writeChan a2z $ SttCruptA2Z_P2A $ (pid, FmpcF2P_WrongFollow)

      FmpcP2F_MyShare sh -> do
        -- Retrieve the simulated share using our mapping
        x <- myShare pid sh
        writeChan a2z $ SttCruptA2Z_P2A (pid, FmpcF2P_MyShare x)

      FmpcP2F_Input _ -> do
        error "Not considering corrupt input party"

  return ()

testMpc2Ideal = runITMinIO 120 $ execUC envTestMPC (idealProtocol) (runMPCFunc 3 1 $ fMPC) simBeaver
