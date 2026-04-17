# Define the compiler and source directories
SRC_DIR = Models
GHC_FLAGS = -i$(SRC_DIR) --make

# Commands:
build: 
	ghc $(GHC_FLAGS) -O -o markov Main.hs

prof:
	ghc $(GHC_FLAGS) -prof -o markov Main.hs

all: build test

# Cleaning commands:
clean:
	rm -f markov
	rm -f *.hi
	rm -f *.o
	rm -f main *.o *.hi $(SRC_DIR)/*.o $(SRC_DIR)/*.hi