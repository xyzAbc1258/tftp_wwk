module Main where

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Network.Socket.Options
import Control.Concurrent
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import System.IO
import System.Timeout
import System.IO.Error
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import Helpers
import Coq_p hiding (bind)

main :: IO ()
main = do
    workerTid <- forkIO tftp
    cmdWorker workerTid

cmdWorker::ThreadId->IO()
cmdWorker tid = do
    cmd <- hGetLine stdin
    case cmd of
        "quit" -> return ()
        _ -> cmdWorker tid

tftp::IO()
tftp = do
    sock <- socket AF_INET Datagram defaultProtocol
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 69 iNADDR_ANY)
    mainLoop sock

mainLoop::Socket->IO()
mainLoop sock = do
    (d,a) <- recvFrom sock 516
    sock2 <- socket AF_INET Datagram defaultProtocol
    putStrLn "Received connection"
    setSocketOption sock2 ReuseAddr 1
    setSocketTimeouts sock2  5000000 0 -- timeout recv 5s
    bind sock2 (SockAddrInet 0 iNADDR_ANY)
    forkIO $ runConn (d,sock2,a)
    mainLoop sock

timedRecv::Socket -> IO Packet -> IO Packet
timedRecv s onNothing = do
    r <- timeout 5000000 $ recvFrom s 516
    case r of
        Just i -> return $ (convertFromBs . fst) i
        Nothing -> onNothing

runConn :: (BS.ByteString, Socket, SockAddr) -> IO ()
runConn (d,s,addr) = do
    let recv = timedRecv s
    let send = \p -> void $ sendTo s (convertToBs p) addr 
    let fixed = fix $ nextWithRecvSend recv send
    fixed Start (convertFromBs d)
    close s
    putStrLn "Closed connection"

nextWithRecvSend::(IO Packet -> IO Packet)->(Packet -> IO())->(State->Packet->IO(State,Packet))->State->Packet->IO(State,Packet)
nextWithRecvSend recv send f s p = do -- recv with continuation
    (ns,np) <- nextStep s p
    when (np /= None) $ send np
    if (ns == Exit) then return (ns,np)
    else do
        let fullRecv = recv $ send np >> fullRecv
        nnp <- fullRecv
        f ns nnp
