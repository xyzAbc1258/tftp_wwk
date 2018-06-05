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
    (ns,np) <- next s p
    when (np /= None) $ send np
    if (ns == Exit) then return (ns,np)
    else do
        let fullRecv = recv $ send np >> fullRecv
        nnp <- fullRecv
        f ns nnp

next::(MonadIO m)=>State->Packet->m(State, Packet)
next Start (RRQ fName) = do
    handle <- liftIO $ tryIOError $ openFile fName ReadMode
    case handle of
        Right fHandle -> next (WAck $ StateInfo fHandle 0) (Ack 0)
        Left e -> return (Exit, Error 0 $ show e) 

next Start (WRQ fName) = do
    liftIO $ putStrLn $ "Opening file " ++ fName
    handle <- liftIO $ tryIOError $ openFile fName WriteMode
    case handle of
        Right fHandle -> (liftIO $ putStrLn $ "Got handle " ++ show fHandle) 
                            >> return (WData $ StateInfo fHandle 1, Ack 0)
        Left e -> return (Exit, Error 0 $ show e)

next (WAck stateInfo) (Ack num) | number stateInfo == num = do
    let fHandle = handle stateInfo
    let currNum = number stateInfo + 1
    r <- liftIO $ BS.hGet fHandle 512
    let stateInfo = StateInfo fHandle currNum
    if (BS.length r) == 512 then return (WAck stateInfo, Data currNum r)
        else do
            liftIO $ hClose fHandle 
            return (Exit, Data currNum r)

next (WData stateInfo) (Data num bs) | number stateInfo == num = do
    let fHandle = handle stateInfo
    let nCount = number stateInfo
    liftIO $ BS.hPut fHandle bs
    liftIO $ putStrLn $ "Writing to file " ++ show fHandle ++ " length: " ++ show (BS.length bs)
    if BS.length bs == 512 then return (WData $ incCounter stateInfo, Ack nCount)
    else do
        liftIO $ hClose fHandle
        return (Exit, Ack nCount)

next c@(WAck stateInfo) (Ack num) | number stateInfo > num = 
    return (c, None)

next c@(WData stateInfo) (Data num bs) | number stateInfo > num = 
    return (c,None)

next s _ = do
    let info = getStateInfo s
    case info of
        Just i -> liftIO $ hClose $ handle i
        _ -> return ()
    return (Exit, Error 0 "Incorrect packet")