Require Import Coq_p.
Require Coq.Strings.String.

Extraction Language Haskell.

Require Import ExtrHaskellBasic.
Require Import ExtrHaskellString.
Require Import ExtrHaskellNatInteger.

Extraction Blacklist String Int List Nat.

Extract Constant Coq_p.openFileMy => 
"\x y -> fmap (\e -> case e of 
              Prelude.Left x -> Prelude.Left (ioeGetErrorString x)
              Prelude.Right x -> Prelude.Right x) 
                (System.IO.Error.tryIOError (System.IO.openFile x y))".
Extract Constant Coq_p.readFileMy => "\x y -> fmap (BS.unpack) (BS.hGet x (Prelude.fromIntegral y))".
Extract Constant Coq_p.writeFileMy => "\x y -> BS.hPut x (BS.pack y)".
Extract Constant Coq_p.closeFileMy => "System.IO.hClose".
Extract Constant Coq_p.Word8 => "Data.Word.Word8".
Extract Constant Coq_p.IOMode => "System.IO.IOMode".
Extract Constant Coq_p.RO => "System.IO.ReadMode".
Extract Constant Coq_p.WO => "System.IO.WriteMode".
Extract Constant Coq_p.Handle => "System.IO.Handle".
Extract Constant Coq_p.dataSize => "512".

Extract Inductive Either => "(Prelude.Either Prelude.String Handle)" [ "Prelude.Left" "Prelude.Right" ].

Extract Inductive IO => "Prelude.IO" ["return"].
Extract Constant bind => "(>>=)".

Module ToExport.
  Definition RO := Coq_p.RO .
  Definition WO := Coq_p.WO .
  Definition dataSize := Coq_p.dataSize.
  Definition openFileMy := Coq_p.openFileMy .
  Definition readFileMy := Coq_p.readFileMy .
  Definition writeFileMy := Coq_p.writeFileMy .
  Definition closeFileMy := Coq_p.closeFileMy .
  Definition Packet := Coq_p.Packet .
  Definition StateInfo := Coq_p.StateInfo .
  Definition getNum := Coq_p.getNum .
  Definition getFileHandle := Coq_p.getFileHandle .
  Definition State := Coq_p.State .
  Definition incNum := Coq_p.incNum .
  Definition nextStep := Coq_p.nextStep .
End ToExport.

Separate Extraction ToExport.
