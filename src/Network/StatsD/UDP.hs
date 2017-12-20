{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.StatsD.UDP
    ( UDPSink
    , udpSend
    , withUDPSink
    )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar.Strict (MVar, newEmptyMVar, takeMVar, putMVar, tryTakeMVar)
import Control.DeepSeq (NFData)
import Control.Exception (SomeException, bracket, catch, finally)

import GHC.Generics (Generic)

import System.IO

import qualified Data.ByteString as B
import qualified Network.Socket as S

data UDPSink
    = UDPSink !(MVar Message)

data Message
    = Write !B.ByteString
    | Close
      deriving (Generic)

instance NFData Message

withUDPSink :: String -> Int -> (UDPSink -> IO a) -> IO a
withUDPSink host port main = do
    inbox <- newEmptyMVar
    _     <- forkIO $ agent host port inbox
    main (UDPSink inbox) `finally` putMVar inbox Close

udpSend :: UDPSink -> B.ByteString -> IO ()
udpSend (UDPSink mailbox) bytes =
    putMVar mailbox $ Write bytes

-- | Background agent moves data from inbox to socket.
agent :: String -> Int -> MVar Message -> IO ()
agent host port inbox = withRestarts inbox $ do
    bracket (initHandle host port) hClose $ \h -> do
        let loop = do msg <- takeMVar inbox
                      case msg of
                          Write b -> B.hPut h b >> loop
                          Close   -> pure ()
        loop

-- | Restarts the agent in case of exceptions. Flushes messages before
-- waiting to retry agent connection loop.
withRestarts :: MVar Message -> IO () -> IO ()
withRestarts inbox work =
    catch work $ \ (_ :: SomeException) -> do
    flush 0 inbox
    threadDelay 1000000 -- 1 sec
    withRestarts inbox work

-- | Drops up to 128 incoming messages to free up writer threads.
flush :: Int -> MVar Message -> IO ()
flush 128 _   = pure ()
flush i inbox = do
    mm <- tryTakeMVar inbox
    case mm of
        Nothing        -> pure ()
        Just Close     -> pure ()
        Just (Write _) -> flush (i + 1) inbox

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
