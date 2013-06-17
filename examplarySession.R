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
	iterationsNo	= 10000,
	strategyNo 	= 2,
	example 	= TRUE	
)

LiangWangExample

LiangWangExample$simulate()

system.time(
	LiangWangExample$simulate()
)

LiangWangExample$stateSpace$dataForPlot
