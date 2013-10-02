CC=ghc

all: air dbsim

air: air.hs
	$(CC) air.hs
	
dbsim: DBsimulation.hs
	$(CC) -main-is DBsimulation.main DBsimulation.hs