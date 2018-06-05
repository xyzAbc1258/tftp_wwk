Require Coq_p.
Require Coq.Strings.String.

Extraction Language Haskell.

Require Import ExtrHaskellBasic.
Require Import ExtrHaskellString.
Require Import ExtrHaskellNatInteger.

Extraction Blacklist String Int List Nat.

Extract Constant Coq_p.openFile => "openFile".

Separate Extraction
         BinNat BinNums BinInt BinPos (* to jest potrzebne do camlcoq *)
         Coq_p.
