rm( list = ls())
directory <- "/home/matteo/Documents/Scienza/Laurea_di_Matematica/Implementation"
setwd(directory)

source("./targetMeasures/targetMeasures.R")
source("./targetMeasures/targetUnnormalisedDensities.R")
source("./targetMeasures/targetLiangDensities.R")
source("./stateSpaces/stateSpaces.R")
source("./stateSpaces/realStateSpaces.R")
source("./algorithms/algorithms.R")
source("./algorithms/parallelTemperings.R")
source("./simulations/simulations.R")


LiangWangExample <- simulation$new(
<<<<<<< HEAD
	iterationsNo	= 100000,
	strategyNo 	= 2,
	example 	= TRUE	
)

LiangWangExample
#LiangWangExample$simulate()

system.time(
  LiangWangExample$simulate()  
) 

LiangWangExample
=======
	iterationsNo= 10,
	strategyNo 	= 3,
	example 	= TRUE
)

LiangWangExample
LiangWangExample$simulate()
LiangWangExample	
LiangWangExample$algorithm$plotHistory()

svg("histogram.svg", width=6, height=4)
	LiangWangExample$algorithm$plotHistory()
dev.off()

head(LiangWangExample$stateSpace$simulatedStates)
>>>>>>> b83e8e7cea6ca75e547504f44dbd2e3570a17439
