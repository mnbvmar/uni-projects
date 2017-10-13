all:
	bnfc lang.cf
	happy -gca ParLang.y
	alex -g LexLang.x
	ghc --make Lang.hs -o Lang

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocLang.* LexLang.* ParLang.* LayoutLang.* SkelLang.* PrintLang.* TestLang.* AbsLang.* TestLang ErrM.* SharedString.* ComposOp.* lang.dtd XMLLang.*
	

