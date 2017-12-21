{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.StatsD.UDP
    ( UDPSink
    , udpPutStrLn
    , withUDPSink
    )
where

import Control.Concurrent             (threadDelay)
import Control.Concurrent.Async       (withAsync)
import Control.Concurrent.MVar.Strict (MVar, newEmptyMVar, takeMVar, putMVar)
import Control.Exception              (IOException, bracket, catch)

import System.IO

import qualified Data.ByteString as B
import qualified Network.Socket  as S

-- | Connection to
data UDPSink = UDPSink !(MVar B.ByteString)

-- | Wrap a computation with a UDPSink to host:port.
withUDPSink :: String -> Int -> (UDPSink -> IO a) -> IO a
withUDPSink host port main = do
    inbox <- newEmptyMVar
    withAsync (withRestarts inbox $ agent host port inbox) $ \_ -> do
        r <- main (UDPSink inbox)
        putStrLn "DONE"
        pure $! r

-- | Send a line of input over UDP.
udpPutStrLn :: UDPSink -> B.ByteString -> IO ()
udpPutStrLn (UDPSink mailbox) = putMVar mailbox

-- | Open UDP socket and copy messages into it.
agent :: String -> Int -> MVar B.ByteString -> IO ()
agent host port inbox = do
    bracket (initHandle host port) hClose $ \h -> do
        let loop = do b <- takeMVar inbox
                      B.hPut h $ b `mappend` "\n"
                      loop
        loop

-- | In case of IO exceptions, sleep for 1s and restart.
withRestarts :: MVar a -> IO () -> IO ()
withRestarts inbox work =
    catch work $ \(err :: IOException) -> do
        hPutStrLn stderr $ show err
        flushSleep inbox
        withRestarts inbox work

-- | Drop messages indefinitely.
flush :: MVar a -> IO ()
flush inbox = do
    _ <- takeMVar inbox
    flush inbox

-- | Drop messages while sleeping for 1s.
flushSleep :: MVar a -> IO ()
flushSleep inbox =
    withAsync (flush inbox) $ \_ -> do
        threadDelay 1000000

-- | Open a UDP socket to host and port.
initHandle :: String -> Int -> IO Handle
initHandle host port = do
    let hints = S.defaultHints { S.addrFlags = [S.AI_PASSIVE] }
    addrInfos <- S.getAddrInfo (Just hints) (Just host) (Just $ show port)
    case addrInfos of
        [] -> error "No address for hostname" -- TODO throw
        (addr:_) -> do
            sock <- S.socket (S.addrFamily addr) S.Datagram S.defaultProtocol
            S.connect sock (S.addrAddress addr)
            h <- S.socketToHandle sock WriteMode
            hSetBuffering h LineBuffering
            pure h
