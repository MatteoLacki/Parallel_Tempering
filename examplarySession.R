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
	iterationsNo	= 100000,
	strategyNo 	= 3,
	example 	= TRUE	
)

LiangWangExample
system.time(
  LiangWangExample$simulate()  
) 
LiangWangExample	
LiangWangExample$algorithm$plotHistory()

svg("histogram.svg", width=6, height=4)
	LiangWangExample$algorithm$plotHistory()
dev.off()