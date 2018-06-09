module Coq_p where

import qualified Prelude
import qualified Datatypes
import qualified PeanoNat
import qualified System.IO
import Control.Monad
import qualified Data.Word
import qualified Data.ByteString as BS
import System.IO.Error


type Handle = System.IO.Handle

type Word8 = Data.Word.Word8

bind :: (Prelude.IO a1) -> (a1 -> Prelude.IO a2) -> Prelude.IO a2
bind = (>>=)

returnA :: a1 -> Prelude.IO a1
returnA a =
  return a

dataSize :: Prelude.Integer
dataSize = 512

type IOMode = System.IO.IOMode

coq_RO :: IOMode
coq_RO = System.IO.ReadMode

coq_WO :: IOMode
coq_WO = System.IO.WriteMode

openFileMy :: Prelude.String -> IOMode -> Prelude.IO
              (Prelude.Either Prelude.String Handle)
openFileMy = \x y -> fmap (\e -> case e of 
              Prelude.Left x -> Prelude.Left (ioeGetErrorString x)
              Prelude.Right x -> Prelude.Right x) 
                (System.IO.Error.tryIOError (System.IO.openFile x y))

readFileMy :: Handle -> Prelude.Integer -> Prelude.IO (([]) Word8)
readFileMy = \x y -> fmap (BS.unpack) (BS.hGet x (Prelude.fromIntegral y))

writeFileMy :: Handle -> (([]) Word8) -> Prelude.IO ()
writeFileMy = \x y -> BS.hPut x (BS.pack y)

closeFileMy :: Handle -> Prelude.IO ()
closeFileMy = System.IO.hClose

data Packet =
   Ack Prelude.Integer
 | Data Prelude.Integer (([]) Word8)
 | RRQ Prelude.String
 | WRQ Prelude.String
 | Error Prelude.Integer Prelude.String
 | None

data StateInfo =
   StateC Handle Prelude.Integer

getNum :: StateInfo -> Prelude.Integer
getNum a =
  case a of {
   StateC _ b -> b}

getFileHandle :: StateInfo -> Handle
getFileHandle a =
  case a of {
   StateC a0 _ -> a0}

data State =
   Start
 | WAck StateInfo
 | WData StateInfo
 | Exit

incNum :: StateInfo -> StateInfo
incNum a =
  case a of {
   StateC a0 b -> StateC a0 ((Prelude.+) b (Prelude.succ 0))}

nextStep :: State -> Packet -> Prelude.IO ((,) State Packet)
nextStep s p =
  case s of {
   Start ->
    case p of {
     RRQ fileName ->
      bind (openFileMy fileName coq_RO) (\a ->
        case a of {
         Prelude.Left s0 -> returnA ((,) Exit (Error (Prelude.succ 0) s0));
         Prelude.Right f ->
          bind (readFileMy f dataSize) (\datas ->
            let {ns = StateC f (Prelude.succ 0)} in
            case (Prelude.==) (Datatypes.length datas) dataSize of {
             Prelude.True ->
              returnA ((,) (WAck ns) (Data (Prelude.succ 0) datas));
             Prelude.False ->
              bind (closeFileMy f) (\_ ->
                returnA ((,) Exit (Data (Prelude.succ 0) datas)))})});
     WRQ fileName ->
      bind (openFileMy fileName coq_WO) (\a ->
        case a of {
         Prelude.Left s0 -> returnA ((,) Exit (Error (Prelude.succ 0) s0));
         Prelude.Right f ->
          returnA ((,) (WData (StateC f (Prelude.succ 0))) (Ack 0))});
     _ ->
      returnA ((,) Exit (Error (Prelude.succ (Prelude.succ 0)) ((:) 'U' ((:)
        'n' ((:) 'r' ((:) 'e' ((:) 'c' ((:) 'o' ((:) 'g' ((:) 'n' ((:) 'i'
        ((:) 'z' ((:) 'e' ((:) 'd' ((:) ' ' ((:) 'p' ((:) 'a' ((:) 'c' ((:)
        'k' ((:) 'e' ((:) 't' ((:) ' ' ((:) 't' ((:) 'y' ((:) 'p' ((:) 'e'
        ([])))))))))))))))))))))))))))};
   WAck s0 ->
    case p of {
     Ack n ->
      case PeanoNat._Nat__ltb (getNum s0) n of {
       Prelude.True ->
        returnA ((,) Exit (Error (Prelude.succ (Prelude.succ (Prelude.succ
          0))) ((:) 'I' ((:) 'n' ((:) 'c' ((:) 'o' ((:) 'r' ((:) 'r' ((:) 'e'
          ((:) 'c' ((:) 't' ((:) ' ' ((:) 'p' ((:) 'a' ((:) 'c' ((:) 'k' ((:)
          'e' ((:) 't' ((:) ' ' ((:) 'n' ((:) 'u' ((:) 'm'
          ([])))))))))))))))))))))));
       Prelude.False ->
        case PeanoNat._Nat__ltb n (getNum s0) of {
         Prelude.True -> returnA ((,) (WAck s0) None);
         Prelude.False ->
          bind (readFileMy (getFileHandle s0) dataSize) (\datas ->
            let {ns = incNum s0} in
            case (Prelude.==) (Datatypes.length datas) dataSize of {
             Prelude.True -> returnA ((,) (WAck ns) (Data (getNum ns) datas));
             Prelude.False ->
              bind (closeFileMy (getFileHandle s0)) (\_ ->
                returnA ((,) Exit (Data (getNum ns) datas)))})}};
     _ ->
      bind (closeFileMy (getFileHandle s0)) (\_ ->
        returnA ((,) Exit (Error (Prelude.succ (Prelude.succ 0)) ((:) 'U'
          ((:) 'n' ((:) 'r' ((:) 'e' ((:) 'c' ((:) 'o' ((:) 'g' ((:) 'n' ((:)
          'i' ((:) 'z' ((:) 'e' ((:) 'd' ((:) ' ' ((:) 'p' ((:) 'a' ((:) 'c'
          ((:) 'k' ((:) 'e' ((:) 't' ((:) ' ' ((:) 't' ((:) 'y' ((:) 'p' ((:)
          'e' ([]))))))))))))))))))))))))))))};
   WData s0 ->
    case p of {
     Data n datas ->
      case PeanoNat._Nat__ltb (getNum s0) n of {
       Prelude.True ->
        returnA ((,) Exit (Error (Prelude.succ (Prelude.succ (Prelude.succ
          0))) ((:) 'I' ((:) 'n' ((:) 'c' ((:) 'o' ((:) 'r' ((:) 'r' ((:) 'e'
          ((:) 'c' ((:) 't' ((:) ' ' ((:) 'p' ((:) 'a' ((:) 'c' ((:) 'k' ((:)
          'e' ((:) 't' ((:) ' ' ((:) 'n' ((:) 'u' ((:) 'm'
          ([])))))))))))))))))))))));
       Prelude.False ->
        case PeanoNat._Nat__ltb n (getNum s0) of {
         Prelude.True -> returnA ((,) (WData s0) None);
         Prelude.False ->
          bind (writeFileMy (getFileHandle s0) datas) (\_ ->
            let {ns = incNum s0} in
            case (Prelude.==) (Datatypes.length datas) dataSize of {
             Prelude.True -> returnA ((,) (WData ns) (Ack (getNum s0)));
             Prelude.False ->
              bind (closeFileMy (getFileHandle s0)) (\_ ->
                returnA ((,) Exit (Ack (getNum s0))))})}};
     _ ->
      bind (closeFileMy (getFileHandle s0)) (\_ ->
        returnA ((,) Exit (Error (Prelude.succ (Prelude.succ 0)) ((:) 'U'
          ((:) 'n' ((:) 'r' ((:) 'e' ((:) 'c' ((:) 'o' ((:) 'g' ((:) 'n' ((:)
          'i' ((:) 'z' ((:) 'e' ((:) 'd' ((:) ' ' ((:) 'p' ((:) 'a' ((:) 'c'
          ((:) 'k' ((:) 'e' ((:) 't' ((:) ' ' ((:) 't' ((:) 'y' ((:) 'p' ((:)
          'e' ([]))))))))))))))))))))))))))))};
   Exit -> returnA ((,) Exit None)}

