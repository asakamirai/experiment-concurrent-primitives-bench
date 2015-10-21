{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Concurrent.Async as AS
import qualified Control.Concurrent.STM   as STM
import qualified Control.Concurrent.MVar  as MVar
import           Control.Concurrent.MVar  (MVar)
import qualified Control.Monad            as M
import           Control.Monad.Primitive  (RealWorld)

import qualified Data.IORef           as Ref
import           Data.IORef           (IORef)
import qualified Data.Atomics         as Atm
import qualified Data.Atomics.Counter as Atm
import qualified Data.Primitive.Array as Arr

import qualified Criterion.Main         as CR
--import qualified Criterion.Main.Options as CR
import qualified Criterion.Types        as CR

-------------------------

noAction :: a -> Int -> IO ()
noAction _v !_c = return ()

readIORef :: IORef Int -> Int -> IO ()
readIORef ref _c = do
    v <- Ref.readIORef ref
    v `seq` return ()

writeIORef :: IORef Int -> Int -> IO ()
writeIORef ref !c = Ref.writeIORef ref c

modifyIORef :: IORef Int -> Int -> IO ()
modifyIORef ref !c = Ref.modifyIORef' ref (const c)

atomicModifyIORef :: IORef Int -> Int -> IO ()
atomicModifyIORef ref !c = Ref.atomicModifyIORef' ref $ const (c, ())

atomicWriteIORef :: IORef Int -> Int -> IO ()
atomicWriteIORef ref !c = Ref.atomicWriteIORef ref c

readForCASIORef :: IORef Int -> Int -> IO ()
readForCASIORef ref _c = do
    ticket <- Atm.readForCAS ref
    let v = Atm.peekTicket ticket
    v `seq` return ()

casIORef :: IORef Int -> Int -> IO ()
casIORef ref !c = do
    ticket <- Atm.readForCAS ref
    (_, next) <- Atm.casIORef ref ticket c
    let v = Atm.peekTicket next
    v `seq` return ()

withIORef :: IO (IORef Int)
withIORef = Ref.newIORef 0

incrCounter :: Atm.AtomicCounter -> Int -> IO ()
incrCounter counter !c = M.void $ Atm.incrCounter c counter

incrCounter_ :: Atm.AtomicCounter -> Int -> IO ()
incrCounter_ counter !c = Atm.incrCounter_ c counter

withAtmCounter :: IO Atm.AtomicCounter
withAtmCounter = Atm.newCounter 0

type Array a = Arr.MutableArray RealWorld a

readArray :: Array Int -> Int -> IO ()
readArray arr !_c = do
    v <- Arr.readArray arr 0
    v `seq` return ()

writeArray :: Array Int -> Int -> IO ()
writeArray arr !c = Arr.writeArray arr 0 c

readArrayElem :: Array Int -> Int -> IO ()
readArrayElem arr !_c = do
    ticket <- Atm.readArrayElem arr 0
    let v = Atm.peekTicket ticket
    v `seq` return ()

casArrayElem :: Array Int -> Int -> IO ()
casArrayElem arr !c = do
    ticket <- Atm.readArrayElem arr 0
    (_, next) <- Atm.casArrayElem arr 0 ticket c
    let v = Atm.peekTicket next
    v `seq` return ()

withMutableArray :: IO (Array Int)
withMutableArray = Arr.newArray 100 0

readMVar :: MVar Int -> Int -> IO ()
readMVar mv _c = do
    v <- MVar.readMVar mv
    v `seq` return ()

tryReadMVar :: MVar Int -> Int -> IO ()
tryReadMVar mv _c = do
    Just v <- MVar.tryReadMVar mv
    v `seq` return ()

takePutMVar :: MVar Int -> Int -> IO ()
takePutMVar mv !c = do
    v <- MVar.takeMVar mv
    MVar.putMVar mv c
    v `seq` return ()

tryTakePutMVar :: MVar Int -> Int -> IO ()
tryTakePutMVar mv !c = do
    Just v <- MVar.tryTakeMVar mv
    M.void $ MVar.tryPutMVar mv c
    v `seq` return ()

swapMVar :: MVar Int -> Int -> IO ()
swapMVar mv !c = do
    v <- MVar.swapMVar mv c
    v `seq` return ()

modifyMVar_ :: MVar Int -> Int -> IO ()
modifyMVar_ mv !c = MVar.modifyMVar_ mv (const $ return c)

modifyMVar :: MVar Int -> Int -> IO ()
modifyMVar mv !c = MVar.modifyMVar mv $ const $ return (c, ())

withMVar :: IO (MVar Int)
withMVar = MVar.newMVar 0

readTVar :: STM.TVar Int -> Int -> IO ()
readTVar tv _c = do
    v <- STM.atomically $ STM.readTVar tv
    v `seq` return ()

writeTVar :: STM.TVar Int -> Int -> IO ()
writeTVar tv !c = STM.atomically $ STM.writeTVar tv c

modifyTVar :: STM.TVar Int -> Int -> IO ()
modifyTVar tv !c = STM.atomically $ STM.modifyTVar tv (const c)

swapTVar :: STM.TVar Int -> Int -> IO ()
swapTVar tv !c = M.void . STM.atomically $ STM.swapTVar tv c

withTVar :: IO (STM.TVar Int)
withTVar = STM.newTVarIO 0

benchRepeat :: Int -> IO v -> (v -> Int -> IO ()) -> IO ()
benchRepeat num newvar act = do
    var <- newvar
    go var num
    where
        go _var 0 = return ()
        go var  c = do
            act var c
            go var $ c - 1

benchConcurrent :: Int -> Int -> IO v -> (v -> Int -> IO ()) -> IO ()
benchConcurrent thnum num newvar act = do
    ass <- M.replicateM thnum $ AS.async $ benchRepeat num newvar act
    M.void $ traverse AS.wait ass

main :: IO ()
main = do
    CR.defaultMainWith configSpeed
        [
          CR.bgroup "none"        $ benchNone benchSpeed
        , CR.bgroup "MVar"        $ benchMVar benchSpeed
        , CR.bgroup "TVar"        $ benchTVar benchSpeed
        , CR.bgroup "IORef"       $ benchIORef benchSpeed
        , CR.bgroup "IORefAtomic" $ benchIORefAtomic benchSpeed
        , CR.bgroup "AtomicCounter" $ benchAtmCounter benchSpeed
        , CR.bgroup "Array"       $ benchArray benchSpeed
        ]
    CR.defaultMainWith configCost
        [
          CR.bgroup "none"        $ benchNone benchCost
        , CR.bgroup "MVar"        $ benchMVar benchCost
        , CR.bgroup "TVar"        $ benchTVar benchCost
        , CR.bgroup "IORef"       $ benchIORef benchCost
        , CR.bgroup "IORefAtomic" $ benchIORefAtomic benchCost
        , CR.bgroup "AtomicCounter" $ benchAtmCounter benchCost
        , CR.bgroup "Array"       $ benchArray benchCost
        ]
    where
        configSpeed = CR.defaultConfig
            { CR.reportFile = Just "report_speed.html" }
        configCost = CR.defaultConfig
            { CR.reportFile = Just "report_cost.html" }
        benchSpeed new = CR.nfIO . benchRepeat 10000 new
        benchCost  new = CR.nfIO . benchConcurrent 10 10000 new
        benchNone bench = [CR.bench "none" $ bench withIORef noAction]
        benchMVar bench =
            [ CR.bench "read"       $ bench withMVar readMVar
            , CR.bench "tryRead"    $ bench withMVar tryReadMVar
            , CR.bench "takePut"    $ bench withMVar takePutMVar
            , CR.bench "tryTakePut" $ bench withMVar tryTakePutMVar
            , CR.bench "swap"       $ bench withMVar swapMVar
            , CR.bench "modify_"    $ bench withMVar modifyMVar_
            , CR.bench "modify"     $ bench withMVar modifyMVar
            ]
        benchTVar bench =
            [ CR.bench "read"   $ bench withTVar readTVar
            , CR.bench "write"  $ bench withTVar writeTVar
            , CR.bench "modify" $ bench withTVar modifyTVar
            , CR.bench "swap"   $ bench withTVar swapTVar
            ]
        benchIORef bench =
            [ CR.bench "read"         $ bench withIORef readIORef
            , CR.bench "write"        $ bench withIORef writeIORef
            , CR.bench "modify"       $ bench withIORef modifyIORef
            , CR.bench "atomicModify" $ bench withIORef atomicModifyIORef
            , CR.bench "atomicWrite"  $ bench withIORef atomicWriteIORef
            ]
        benchAtmCounter bench =
            [ CR.bench "incrCounter"  $ bench withAtmCounter incrCounter
            , CR.bench "incrCounter_" $ bench withAtmCounter incrCounter_
            ]
        benchIORefAtomic bench =
            [ CR.bench "readForCAS" $ bench withIORef readForCASIORef
            , CR.bench "cas"        $ bench withIORef casIORef
            ]
        benchArray bench =
            [ CR.bench "readArray"     $ bench withMutableArray readArray
            , CR.bench "writeArray"    $ bench withMutableArray writeArray
            , CR.bench "readArrayElem" $ bench withMutableArray readArrayElem
            , CR.bench "casArrayElem"  $ bench withMutableArray casArrayElem
            ]


