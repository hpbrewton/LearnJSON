main: obsTable
	./main

obsTable: tree ObservationTable.hs
	ghc -o main tree teacher ObservationTable.hs 

tree: Tree.hs
	ghc Tree.hs

teacher: tree Teacher.hs 
	ghc Teacher.hs

.PHONY: clean 
clean: 
	-rm *.hi *.o 
	-rm tree 
	-rm main 