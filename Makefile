all: extract 
	sed -ie '/^import qualified PeanoNat/a import qualified System.IO\nimport Control.Monad\nimport qualified Data.Word\nimport qualified Data.ByteString as BS\nimport System.IO.Error\n' Coq_p.hs
	rm -f Top.hs
	mv -f *.hs ./src
	stack build

extract: extraction.v Coq_p.vo
	coqtop -batch -load-vernac-source $<

%.vo: %.v
	coqc $<
		
clean:
	rm -f *.o *.cmi *.cmo *.cmx *.vo *.hi *.hsi *.hs *.glob *.hse .Coq_p.vo.aux
	
