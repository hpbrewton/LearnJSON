main: algebra obsTable tree treeAut teacher 
	ghc -o main  BooleanAlgebra TreeAutomata Tree Teacher ObservationTable Main.hs
	./main 

treeAut: algebra TreeAutomata.hs 
	ghc -v BooleanAlgebra TreeAutomata.hs 

algebra: BooleanAlgebra.hs
	ghc BooleanAlgebra.hs 

obsTable: tree teacher treeAut ObservationTable.hs
	ghc TreeAutomata Tree Teacher ObservationTable.hs 

tree: Tree.hs
	ghc Tree.hs

teacher: tree Teacher.hs 
	ghc Tree TreeAutomata Teacher.hs

.PHONY: clean 
clean: 
	-rm *.hi *.o 
	-rm tree 
	-rm main 
	-rm BooleanAlgebra
	-rm TreeAutomata