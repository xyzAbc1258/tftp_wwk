Require Import Coq.Arith.PeanoNat.
Require Import PeanoNat.
Require Import Coq.NArith.BinNat.
Include Coq.Init.Nat.
Require Import NAxioms NProperties OrdersFacts.
Require Import Ascii.
Require Import Coq.Vectors.Vector.
Require Import Coq.Strings.String.
Require Import Coq.Lists.List.

Definition Handle := nat.

Definition Word8 := nat.

Inductive Either : Set :=
| Left: string -> Either
| Right: Handle -> Either.

Inductive IO (A:Type) := IOc (a : A).

Definition bind {A B : Type} (a : IO A) (f : A -> IO B) : IO B :=
  match a with
  | IOc _ a => f a
  end.

Definition returnA {A : Type} (a : A) : IO A :=
  IOc A a.

Axiom dataSize : nat.

Definition IOMode := nat.
Axiom RO : IOMode.
Axiom WO : IOMode.
(*filename->tryb*)
Axiom openFileMy:string-> IOMode -> IO Either.
Axiom readFileMy:Handle->nat-> IO (list Word8).
Axiom writeFileMy:Handle->list Word8-> IO (unit).
Axiom closeFileMy:Handle-> IO(unit).


Lemma read_less_than_arg :
  forall (h : Handle) (l : nat), match (readFileMy h l) with | IOc _ a => length a <= l end.
Proof.
Admitted.

Inductive Packet : Set :=
| Ack : nat -> Packet
| Data : nat -> list Word8 -> Packet
| RRQ : string -> Packet
| WRQ : string -> Packet
| Error : nat -> string -> Packet
| None : Packet .

(*fileHandle->number*)
Inductive StateInfo : Type := 
StateC : Handle -> nat -> StateInfo.

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



Definition nextStep (s : State) (p : Packet) : IO (State * Packet) :=
  match s,p with
  | Start, RRQ fileName => bind (openFileMy fileName RO) (fun a => match a with
                               | Right f => bind (readFileMy f dataSize) ( fun datas =>
                                    let ns := StateC f 1 in
                                    if(length datas =? dataSize) then returnA (WAck ns, Data 1 datas)
                                    else bind (closeFileMy f) ( fun _ =>
                                        returnA (Exit, Data 1 datas)))
                               | Left s => returnA (Exit, Error 1 s)
                               end)
  | Start, WRQ fileName => bind (openFileMy fileName WO) (fun a => match a with
                               | Right f => returnA (WData (StateC f 1), Ack 0)
                               | Left s => returnA (Exit, Error 1 s)
                               end)
  | WAck s, Ack n => if ((getNum s) <? n) then returnA (Exit, Error 3 "Incorrect packet num")
                     else if (n <? getNum s) then returnA (WAck s, None)
                     else bind (readFileMy (getFileHandle s) dataSize) ( fun datas =>
                          let ns := incNum s in
                          if(length datas =? dataSize) then returnA (WAck ns, Data (getNum ns) datas)
                          else bind (closeFileMy (getFileHandle s)) ( fun _ =>
                                returnA (Exit, Data (getNum ns) datas)))
  | WData s, Data n datas => if ((getNum s) <? n) then returnA (Exit, Error 3 "Incorrect packet num")
                             else if (n <? getNum s) then returnA (WData s, None)
                             else bind (writeFileMy (getFileHandle s) datas) (fun _ =>
                                  let ns := incNum s in
                                  if(length datas =? dataSize) then returnA (WData ns, Ack (getNum s))
                                  else bind (closeFileMy (getFileHandle s)) ( fun _ => returnA
                                        (Exit, Ack (getNum s))))
  | Start,_ => returnA (Exit, Error 2 "Unrecognized packet type")
  | WAck s, _ =>bind (closeFileMy (getFileHandle s)) (
 fun x => returnA (Exit, Error 2 "Unrecognized packet type"))
  | WData s, _ =>  bind (closeFileMy (getFileHandle s)) (fun _ => returnA (Exit, Error 2 "Unrecognized packet type"))
  | Exit, _ => returnA (Exit, None)  
  end.

Definition isExitError (a : IO (State * Packet)) : Prop :=
  match a with
  | IOc _ (Exit, Error _ _) => True
  | _ => False
  end.

Definition isWRQ_RRQ (a: Packet) : Prop :=
  match a with
  | WRQ _ => True
  | RRQ _ => True
  | _ => False
  end.

Theorem Start_only_with_RRQ_WRQ : 
  forall p:Packet, (not (isWRQ_RRQ p)) -> isExitError (nextStep Start p).
Proof.
  intros p.
  unfold not, isWRQ_RRQ, isExitError.
  simpl.
  destruct p;unfold returnA; eauto; intros a; destruct a; auto.
Qed.

Definition is_IO_Left (a : IO Either) :=
  match a with
  | IOc _ (Left _) => True
  | _ => False
  end.

Definition is_IO_Right (a : IO Either) :=
  match a with
  | IOc _ (Right _) => True
  | _ => False
  end.

Theorem Exit_on_open_error_RO :
  forall (fileName : string), 
    is_IO_Left (openFileMy fileName RO) -> isExitError (nextStep Start (RRQ fileName)).
Proof.
  intros fileName.
  unfold is_IO_Left, nextStep, isExitError, returnA, bind.
  destruct (openFileMy fileName RO).
  destruct a; eauto; intros f; inversion f.
Qed.

Theorem Exit_on_open_error_WO :
  forall (fileName : string), 
    is_IO_Left (openFileMy fileName WO) -> isExitError (nextStep Start (WRQ fileName)).
Proof.
  intros fileName.
  unfold is_IO_Left, nextStep, isExitError, returnA, bind.
  destruct (openFileMy fileName WO).
  destruct a; eauto; intros f; inversion f.
Qed.

Definition is_Exit_On_Read_Less_Than_512B (a : IO (State * Packet)) :=
  match a with
  | IOc _ (Exit, Data _ d) => length d < dataSize
  | _ => True
  end.

Lemma not_le_self : forall (n : nat), n <? n = false.
Proof.
  intros n.
  induction n.
  auto.
  auto.
Qed.

Lemma le_lt_or_eq : forall (n m :nat), n <= m -> n < m \/ n = m.
Proof.
  intros n m.
  intros H.
  destruct H; try auto.
  pose proof (Lt.le_lt_n_Sm n m) as p.
  auto.
Qed.

Lemma le_if_leq_and_neq : forall (n : nat) (m : nat), n <= m /\ n <> m -> n < m.
Proof.
  intros.
  pose proof (le_lt_or_eq n m) as p.
  intuition.
Qed.

Lemma eqn : forall (a: nat) (b : nat), (a =? b) = false <-> a <> b.
Proof.
  intros a b.
  pose proof (Nat.eqb_eq a b) as p.
  destruct p.
  remember (a =? b) as eq.
  destruct eq; intuition.
Qed.

Theorem Exit_on_read_Less_Than_512B :
  forall (n : nat) (handle : Handle),
    is_Exit_On_Read_Less_Than_512B (nextStep (WAck (StateC handle n)) (Ack n)).
Proof.
  intros n handle.
  pose proof (not_le_self n) as pp.
  unfold is_Exit_On_Read_Less_Than_512B, nextStep, returnA, bind.
  simpl.
  destruct (n <? n); try  discriminate.
  pose proof (read_less_than_arg handle dataSize) as a_l_leq_ds.
  destruct (readFileMy handle dataSize).
  remember (Datatypes.length a =? dataSize) as a_l_eq_ds.
  destruct a_l_eq_ds; auto.
  remember (closeFileMy handle) as cl.
  destruct cl.
  remember (Datatypes.length a) as len_a.
  pose proof (le_if_leq_and_neq len_a dataSize) as ld.
  cut (len_a <> dataSize).
  + intros neq_a.
    apply ld.
    auto.
  + pose proof (eqn len_a dataSize) as p.
    apply p.
    symmetry.
    assumption.
Qed.
    

Theorem Exit_on_write_Less_Than_512B :
  forall (n : nat) (handle : Handle) (d : list Word8),
    length d <= dataSize -> (match (nextStep (WData (StateC handle n)) (Data n d))with 
      | IOc _ (Exit, Ack n) => length d < dataSize | _ => True end).
Proof.
  intros.
  unfold nextStep, bind, returnA.
  simpl.
  destruct (n <? n); auto.
  destruct (writeFileMy handle d).
  remember (Datatypes.length d =? dataSize) as a_l_eq_ds.
  destruct a_l_eq_ds; auto.
  remember (closeFileMy handle) as cl.
  destruct cl.
  remember (Datatypes.length d) as len_a.
  pose proof (le_if_leq_and_neq len_a dataSize) as ld.
  cut (len_a <> dataSize).
  + intros neq_a.
    apply ld.
    auto.
  + pose proof (eqn len_a dataSize) as p.
    apply p.
    symmetry.
    assumption.
Qed.
    

Theorem WRQ_reponds_Ack_0 : 
  forall (fileName : string),
    is_IO_Right (openFileMy fileName WO) -> (match (nextStep Start (WRQ fileName)) with 
    | IOc _ (WData _, Ack 0) => True | _ => False end).
Proof.
  intros fileName.
  unfold is_IO_Right, nextStep, bind, returnA.
  destruct (openFileMy fileName WO).
  destruct a; auto.
Qed.

Theorem NextStep_increment_number_WAck :
  forall (n : nat) (h: Handle),
  match (nextStep (WAck (StateC h n)) (Ack n)) with 
    | IOc _ (WAck s, _) => getNum s = (n+1) | _ => True end.
Proof.
  intros n h.
  unfold nextStep, bind, returnA, getNum.
  simpl.
  destruct (n <? n); auto.
  destruct (readFileMy h dataSize).
  destruct (closeFileMy h).
  destruct (Datatypes.length a =? dataSize); auto.
Qed.

Theorem NextStep_increment_number_WData :
  forall (n : nat) (h: Handle) (d : list Word8),
  match (nextStep (WData (StateC h n)) (Data n d)) with 
    | IOc _ (WData s, _) => getNum s = (n+1) | _ => True end.
Proof.
  intros n h d.
  intros.
  unfold nextStep, bind, returnA, getNum.
  simpl.
  destruct (n <? n); auto.
  destruct (writeFileMy h d).
  destruct (closeFileMy h).
  destruct (Datatypes.length d =? dataSize); auto.
Qed.

Theorem Old_packets_are_ignored_WAck :
  forall (n nold : nat) (h : Handle),
  nold < n -> match nextStep (WAck (StateC h n)) (Ack nold) with
  | IOc _ (WAck (StateC h n), None) => True | _ => False end.
Proof.
  intros n nold h.
  intros.
  unfold nextStep, bind, returnA, getNum.
  simpl.
  pose proof (Nat.ltb_lt n nold) as p.
  remember (n <? nold) as nn.
  destruct nn.
  + destruct p.
    pose proof (Nat.lt_asymm n nold) as assym.
    apply assym; try assumption.
    apply H0.
    auto.
  + pose proof (Nat.ltb_lt nold n) as p2.
    destruct p2.
    destruct (nold <? n); auto.
    pose proof (H1 H) as f.
    discriminate.
Qed.

Theorem Old_packets_are_ignored_WData :
  forall (n nold : nat) (h : Handle) (data: list Word8),
  nold < n -> match nextStep (WData (StateC h n)) (Data nold data) with
  | IOc _ (WData (StateC h n), None) => True | _ => False end.
Proof.
  intros n nold h data.
  intros.
  unfold nextStep, bind, returnA, getNum.
  simpl.
  pose proof (Nat.ltb_lt n nold) as p.
  remember (n <? nold) as nn.
  destruct nn.
  + destruct p.
    pose proof (Nat.lt_asymm n nold) as assym.
    apply assym; try assumption.
    apply H0.
    auto.
  + pose proof (Nat.ltb_lt nold n) as p2.
    destruct p2.
    destruct (nold <? n); auto.
    pose proof (H1 H) as f.
    discriminate.
Qed.
    
  




  
  
