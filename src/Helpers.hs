module Helpers
where 

import Network.Socket.ByteString
import System.IO
import Control.Monad.IO.Class
import Data.Maybe
import System.IO.Error
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import Coq_p

instance Eq StateInfo where
  (StateC f1 f2) == (StateC s1 s2) = f1 == s1 && f2 == s2
  t1 /= t2 = not (t1 == t2)

instance Eq State where
  (/=) f s = not (f == s)
  Exit == Exit = True
  Start == Start = True
  WAck f1 == WAck s1 = f1 == s1
  WData f1 == WData f2 = f1 == f2
  _ == _ = False
  
instance Eq Packet where
  (/=) f s = not (s == f)
  None == None = True
  (Ack i1) == (Ack i2) = i1 == i2
  (Data i1 l1) == (Data i2 l2) = i1 == i2 && l1 == l2
  (RRQ f1) == (RRQ f2) = f1 == f2
  (WRQ f1) == (WRQ f2) = f1 == f2
  (Error i1 e1) == (Error i2 e2) = i1 == i2 && e1 == e2
  _ == _ = False

getStateInfo::State->Maybe StateInfo
getStateInfo (WAck s) = Just s
getStateInfo (WData s) = Just s
getStateInfo _ = Nothing

intToBytes::Int -> [Int]
intToBytes i = [i `div` 256 `mod` 256, i `mod` 256]
intToBs i = BS.pack $ map fromIntegral $ intToBytes i

convertToBs::Packet -> BS.ByteString
convertToBs (Ack i) = BS.pack $ map fromIntegral (0:4:(intToBytes (fromIntegral i)))
convertToBs (Data i bs) = BS.append (BS.pack [0,3]) $ BS.append (intToBs (fromIntegral i)) (BS.pack bs)
convertToBs (Error i str) = flip BS.append (BS.singleton 0) $ 
    BS.append (BS.pack [0,5]) $ BS.append (intToBs (fromIntegral i)) $ BS.pack $ map BS.c2w str
convertToBs None = BS.empty

convertFromBs::BS.ByteString -> Packet
convertFromBs bs = let (s,r) = fromJust $ BS.uncons $ BS.tail bs in
    case s of
        1 -> RRQ fileName 
            where fileName = map BS.w2c $ takeWhile (/= 0) $ BS.unpack r
        2 -> WRQ fileName
            where fileName = map BS.w2c $ takeWhile (/= 0) $ BS.unpack r
        3 -> Data num dat
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
