module Coq_p where

import qualified Prelude
import qualified Datatypes
import qualified PeanoNat

data Either =
   Left Prelude.String
 | Right Prelude.Integer

coq_Either_rect :: (Prelude.String -> a1) -> (Prelude.Integer -> a1) -> Either ->
                   a1
coq_Either_rect f f0 e =
  case e of {
   Left x -> f x;
   Right x -> f0 x}

coq_Either_rec :: (Prelude.String -> a1) -> (Prelude.Integer -> a1) -> Either ->
                  a1
coq_Either_rec =
  coq_Either_rect

openFile :: Prelude.String -> Prelude.Integer -> Either
openFile = openFile

readFile :: Prelude.Integer -> Prelude.Integer -> ([]) Prelude.Integer
readFile =
  Prelude.error "AXIOM TO BE REALIZED"

writeFile :: Prelude.Integer -> (([]) Prelude.Integer) -> ()
writeFile =
  Prelude.error "AXIOM TO BE REALIZED"

closeFile :: Prelude.Integer -> ()
closeFile =
  Prelude.error "AXIOM TO BE REALIZED"

data Packet =
   Ack Prelude.Integer
 | Data Prelude.Integer (([]) Prelude.Integer)
 | RRQ Prelude.String
 | WRQ Prelude.String
 | Error Prelude.Integer Prelude.String
 | None

coq_Packet_rect :: (Prelude.Integer -> a1) -> (Prelude.Integer -> (([])
                   Prelude.Integer) -> a1) -> (Prelude.String -> a1) ->
                   (Prelude.String -> a1) -> (Prelude.Integer -> Prelude.String
                   -> a1) -> a1 -> Packet -> a1
coq_Packet_rect f f0 f1 f2 f3 f4 p =
  case p of {
   Ack x -> f x;
   Data x x0 -> f0 x x0;
   RRQ x -> f1 x;
   WRQ x -> f2 x;
   Error x x0 -> f3 x x0;
   None -> f4}

coq_Packet_rec :: (Prelude.Integer -> a1) -> (Prelude.Integer -> (([])
                  Prelude.Integer) -> a1) -> (Prelude.String -> a1) ->
                  (Prelude.String -> a1) -> (Prelude.Integer -> Prelude.String ->
                  a1) -> a1 -> Packet -> a1
coq_Packet_rec =
  coq_Packet_rect

data StateInfo =
   StateC Prelude.Integer Prelude.Integer

coq_StateInfo_rect :: (Prelude.Integer -> Prelude.Integer -> a1) -> StateInfo ->
                      a1
coq_StateInfo_rect f s =
  case s of {
   StateC x x0 -> f x x0}

coq_StateInfo_rec :: (Prelude.Integer -> Prelude.Integer -> a1) -> StateInfo ->
                     a1
coq_StateInfo_rec =
  coq_StateInfo_rect

getNum :: StateInfo -> Prelude.Integer
getNum a =
  case a of {
   StateC _ b -> b}

getFileHandle :: StateInfo -> Prelude.Integer
getFileHandle a =
  case a of {
   StateC a0 _ -> a0}

data State =
   Start
 | WAck StateInfo
 | WData StateInfo
 | Exit

coq_State_rect :: a1 -> (StateInfo -> a1) -> (StateInfo -> a1) -> a1 -> State ->
                  a1
coq_State_rect f f0 f1 f2 s =
  case s of {
   Start -> f;
   WAck x -> f0 x;
   WData x -> f1 x;
   Exit -> f2}

coq_State_rec :: a1 -> (StateInfo -> a1) -> (StateInfo -> a1) -> a1 -> State ->
                 a1
coq_State_rec =
  coq_State_rect

incNum :: StateInfo -> StateInfo
incNum a =
  case a of {
   StateC a0 b -> StateC a0 ((Prelude.+) b (Prelude.succ 0))}

nextStep :: State -> Packet -> (,) State Packet
nextStep s p =
  case s of {
   Start ->
    case p of {
     RRQ fileName ->
      case openFile fileName (Prelude.succ 0) of {
       Left s0 -> (,) Exit (Error (Prelude.succ 0) s0);
       Right f -> (,) (WData (StateC f 0)) (Ack 0)};
     WRQ fileName ->
      case openFile fileName (Prelude.succ (Prelude.succ 0)) of {
       Left s0 -> (,) Exit (Error (Prelude.succ 0) s0);
       Right f -> (,) (WData (StateC f 0)) (Ack 0)};
     _ -> (,) Exit (Error (Prelude.succ (Prelude.succ 0)) ((:) 'U' ((:) 'n' ((:)
      'r' ((:) 'e' ((:) 'c' ((:) 'o' ((:) 'g' ((:) 'n' ((:) 'i' ((:) 'z' ((:) 'e'
      ((:) 'd' ((:) ' ' ((:) 'p' ((:) 'a' ((:) 'c' ((:) 'k' ((:) 'e' ((:) 't'
      ((:) ' ' ((:) 't' ((:) 'y' ((:) 'p' ((:) 'e' ([]))))))))))))))))))))))))))};
   WAck s0 ->
    case p of {
     Ack n ->
      case PeanoNat._Nat__ltb (getNum s0) n of {
       Prelude.True -> (,) Exit (Error (Prelude.succ (Prelude.succ (Prelude.succ
        0))) ((:) 'I' ((:) 'n' ((:) 'c' ((:) 'o' ((:) 'r' ((:) 'r' ((:) 'e' ((:)
        'c' ((:) 't' ((:) ' ' ((:) 'p' ((:) 'a' ((:) 'c' ((:) 'k' ((:) 'e' ((:)
        't' ((:) ' ' ((:) 'n' ((:) 'u' ((:) 'm' ([]))))))))))))))))))))));
       Prelude.False ->
        case PeanoNat._Nat__ltb n (getNum s0) of {
         Prelude.True -> (,) (WAck s0) None;
         Prelude.False ->
          let {
           datas = readFile (getFileHandle s0) (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                     (Prelude.succ (Prelude.succ
                     0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))}
          in
          let {ns = incNum s0} in
          case (Prelude.==) (Datatypes.length datas) (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ
                 0)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) of {
           Prelude.True -> (,) (WAck ns) (Data (getNum ns) datas);
           Prelude.False -> (,) Exit (Data (getNum ns) datas)}}};
     _ -> (,) Exit (Error (Prelude.succ (Prelude.succ 0)) ((:) 'U' ((:) 'n' ((:)
      'r' ((:) 'e' ((:) 'c' ((:) 'o' ((:) 'g' ((:) 'n' ((:) 'i' ((:) 'z' ((:) 'e'
      ((:) 'd' ((:) ' ' ((:) 'p' ((:) 'a' ((:) 'c' ((:) 'k' ((:) 'e' ((:) 't'
      ((:) ' ' ((:) 't' ((:) 'y' ((:) 'p' ((:) 'e' ([]))))))))))))))))))))))))))};
   WData s0 ->
    case p of {
     Data n datas ->
      case PeanoNat._Nat__ltb (getNum s0) n of {
       Prelude.True -> (,) Exit (Error (Prelude.succ (Prelude.succ (Prelude.succ
        0))) ((:) 'I' ((:) 'n' ((:) 'c' ((:) 'o' ((:) 'r' ((:) 'r' ((:) 'e' ((:)
        'c' ((:) 't' ((:) ' ' ((:) 'p' ((:) 'a' ((:) 'c' ((:) 'k' ((:) 'e' ((:)
        't' ((:) ' ' ((:) 'n' ((:) 'u' ((:) 'm' ([]))))))))))))))))))))));
       Prelude.False ->
        case PeanoNat._Nat__ltb n (getNum s0) of {
         Prelude.True -> (,) (WData s0) None;
         Prelude.False ->
          let {ns = incNum s0} in
          case (Prelude.==) (Datatypes.length datas) (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                 (Prelude.succ (Prelude.succ
                 0)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) of {
           Prelude.True -> (,) (WData ns) (Ack (getNum s0));
           Prelude.False -> (,) Exit (Ack (getNum s0))}}};
     _ -> (,) Exit (Error (Prelude.succ (Prelude.succ 0)) ((:) 'U' ((:) 'n' ((:)
      'r' ((:) 'e' ((:) 'c' ((:) 'o' ((:) 'g' ((:) 'n' ((:) 'i' ((:) 'z' ((:) 'e'
      ((:) 'd' ((:) ' ' ((:) 'p' ((:) 'a' ((:) 'c' ((:) 'k' ((:) 'e' ((:) 't'
      ((:) ' ' ((:) 't' ((:) 'y' ((:) 'p' ((:) 'e' ([]))))))))))))))))))))))))))};
   Exit -> (,) Exit None}

