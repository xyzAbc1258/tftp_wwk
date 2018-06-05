module Helpers
where 

import Network.Socket.ByteString
import System.IO
import Control.Monad.IO.Class
import Data.Maybe
import System.IO.Error
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS (c2w, w2c)

data StateInfo = StateInfo {
    handle::Handle,
    number::Int
} deriving Eq

data State = Start
    | WAck StateInfo
    | WData StateInfo
    | Exit
    deriving Eq

data Packet = Ack Int
    | Data Int BS.ByteString
    | RRQ String
    | WRQ String
    | Error Int String
    | None
    deriving Eq

getStateInfo::State->Maybe StateInfo
getStateInfo (WAck s) = Just s
getStateInfo (WData s) = Just s
getStateInfo _ = Nothing

intToBytes::Int -> [Int]
intToBytes i = [i `div` 256 `mod` 256, i `mod` 256]
intToBs i = BS.pack $ map fromIntegral $ intToBytes i

convertToBs::Packet -> BS.ByteString
convertToBs (Ack i) = BS.pack $ map fromIntegral (0:4:(intToBytes i))
convertToBs (Data i bs) = BS.append (BS.pack [0,3]) $ BS.append (intToBs i) bs
convertToBs (Error i str) = flip BS.append (BS.singleton 0) $ 
    BS.append (BS.pack [0,5]) $ BS.append (intToBs i) $ BS.pack $ map BS.c2w str
convertToBs None = BS.empty

convertFromBs::BS.ByteString -> Packet
convertFromBs bs = let (s,r) = fromJust $ BS.uncons $ BS.tail bs in
    case s of
        1 -> RRQ fileName 
            where fileName = map BS.w2c $ takeWhile (/= 0) $ BS.unpack r
        2 -> WRQ fileName
            where fileName = map BS.w2c $ takeWhile (/= 0) $ BS.unpack r
        3 -> Data num (BS.pack dat)
            where num = (fromIntegral f) * 256 + (fromIntegral s)
                  (f:s:dat) = BS.unpack r
        4 -> Ack num
            where num = (fromIntegral f) * 256 + (fromIntegral s)
                  (f:s:[]) = BS.unpack r
        5 -> Error num msg
            where num = (fromIntegral f) * 256 + (fromIntegral s)
                  msg = map BS.w2c $ takeWhile (/= 0) rest
                  (f:s:rest) = BS.unpack r
        _ -> None

incCounter::StateInfo-> StateInfo
incCounter stateInfo = 
    StateInfo (handle stateInfo) (number stateInfo + 1)