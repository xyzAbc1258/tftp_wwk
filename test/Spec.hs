import Helpers
import System.IO
import Control.Monad
import Control.Monad.Except
import Network.Socket.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS (c2w, w2c)


main :: IO ()
main = 
    checkIO

checkIO:: IO ()
checkIO = do
    r <- runExceptT check
    case r of
        Left m -> putStrLn $ "Failed - " ++ m
        Right _ -> putStrLn "OK"

check::ExceptT String IO ()
check = do
    checkParsings

checkParsings::ExceptT String IO ()
checkParsings = do
    expects (convertFromBSHand sampleAck) ackExpected "Ack From"
    expects (convertBack sampleAck) sampleAck "Ack To"
    expects (convertFromBSHand sampleRRQ) rrqExpected "RRQ From"
    expects (convertFromBSHand sampleWRQ) wrqExpected "WRQ From"
    expects (convertFromBSHand sampleData) dataExpected "Data From"
    expects (convertBack sampleData) sampleData "Data To"
    expects (convertFromBSHand sampleError) errorExpected "Error From"
    expects (convertBack sampleError) sampleError "Error To"

convertBack = convertToBs . convertFromBSHand
convertFromBSHand = convertFromBs

sampleHandle::Handle
sampleHandle = stdout

expects::Eq a => a->a->String-> ExceptT String IO ()
expects a b msg =
    when (a /= b) $ throwError msg

sampleRRQ::BS.ByteString
sampleRRQ = BS.pack $ [0,1] ++ (fmap BS.c2w "filename") ++ [0] ++ (fmap BS.c2w "ascii") ++ [0]

rrqExpected::Packet
rrqExpected = RRQ "filename"

sampleWRQ::BS.ByteString
sampleWRQ = BS.pack $ [0,2] ++ (fmap BS.c2w "filename") ++ [0] ++ (fmap BS.c2w "ascii") ++ [0]

wrqExpected::Packet
wrqExpected = WRQ "filename"

sampleDataContent = [0..255] ++ [0..255] ++ [251]

sampleData::BS.ByteString
sampleData = BS.pack $ [0,3,2,5] ++ sampleDataContent

dataExpected::Packet
dataExpected = Data 517 $ BS.pack sampleDataContent

sampleAck::BS.ByteString
sampleAck = BS.pack $ [0,4,1,10]

ackExpected::Packet
ackExpected = Ack 266

errMsg = "Error messageekjksjdks"

sampleError::BS.ByteString
sampleError = BS.pack $ [0,5,0,5] ++ (fmap BS.c2w errMsg) ++ [0]

errorExpected:: Packet
errorExpected = Error 5 errMsg