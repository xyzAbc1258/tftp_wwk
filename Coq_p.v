Require Import Coq.Arith.PeanoNat.
Require Import Ascii.
Require Import Coq.Vectors.Vector.
Require Import Coq.Strings.String.
Require Import Coq.Lists.List.

Inductive Either : Set :=
| Left: string -> Either
| Right: nat -> Either.

(*filename->tryb*)
Axiom openFile:string-> nat -> Either.
Axiom readFile:nat->nat->list nat.
Axiom writeFile:nat->list nat-> unit.
Axiom closeFile:nat->unit.
Lemma noLongerRead: forall x y : nat, length (readFile x y) <= y.
Proof.
Admitted.



Inductive Packet : Set :=
| Ack : nat -> Packet
| Data : nat -> list nat -> Packet
| RRQ : string -> Packet
| WRQ : string -> Packet
| Error : nat -> string -> Packet
| None : Packet .

(*fileHandle->number*)
Inductive StateInfo : Type := 
StateC : nat -> nat -> StateInfo.

Definition getNum (a: StateInfo) :=
  match a with
  | StateC a b => b
  end.

Definition getFileHandle (a: StateInfo) :=
  match a with
  | StateC a b => a
  end.

Inductive State : Set :=
| Start
| WAck : StateInfo -> State
| WData : StateInfo -> State
| Exit.

Definition incNum (a : StateInfo) :=
  match a with
  | StateC a b => StateC a (b+1)
  end.

Lemma incNumP: forall a:StateInfo, (getNum (incNum a)) = ((getNum a) +1).
Proof.
  intros.
  destruct a.
  unfold getNum.
  eauto.
Qed.



Definition nextStep (s : State) (p : Packet) :=
  match s,p with
  | Start, RRQ fileName => match (openFile fileName 1) with
                               | Right f => ((WData (StateC f 0)), (Ack 0))
                               | Left s => (Exit, Error 1 s)
                               end
  | Start, WRQ fileName => match (openFile fileName 2) with
                               | Right f => (WData (StateC f 0), Ack 0)
                               | Left s => (Exit, Error 1 s)
                               end
  | WAck s, Ack n => if ((getNum s) <? n) then (Exit, Error 3 "Incorrect packet num")
                     else if (n <? getNum s) then (WAck s, None)
                     else let datas := readFile (getFileHandle s) 512 in
                          let ns := incNum s in
                          if(length datas =? 512) then (WAck ns, Data (getNum ns) datas)
                          else let _ := closeFile (getFileHandle s) in
                                (Exit, Data (getNum ns) datas)
  | WData s, Data n datas => if ((getNum s) <? n) then (Exit, Error 3 "Incorrect packet num")
                             else if (n <? getNum s) then (WData s, None)
                             else let _ := writeFile (getFileHandle s) datas in
                                  let ns := incNum s in
                                  if(length datas =? 512) then (WData ns, Ack (getNum s))
                                  else let _ := closeFile (getFileHandle s) in
                                        (Exit, Ack (getNum s))
  | Start,_ =>  (Exit, Error 2 "Unrecognized packet type")
  | WAck s, _ => let _ := closeFile (getFileHandle s) in (Exit, Error 2 "Unrecognized packet type")
  | WData s, _ =>  let _ := closeFile (getFileHandle s) in (Exit, Error 2 "Unrecognized packet type")
  | Exit, _ => (Exit, None)  
  end.

