rm( list = ls())
directory <- "/home/matteo/Documents/Scienza/Laurea_di_Matematica/Implementation"
setwd(directory)

source("./referenceObjects/targetMeasures.R")
source("./referenceObjects/targetUnnormalisedDensities.R")
source("./referenceObjects/targetLiangDensities.R")
source("./referenceObjects/stateSpace.R")
source("./referenceObjects/realStateSpace.R")
source("./referenceObjects/algorithm.R")
source("./referenceObjects/parallelTempering.R")
source("./referenceObjects/simulations.R")

LiangWangExample <- simulation$new(
	iterationsNo	= 10,
	strategyNo 	= 2,
	example 	= TRUE	
)

LiangWangExample$simulate()


LiangWangExample$stateSpace$simulationTerminated()

LiangWangExample$stateSpace$dataForPlot

LiangWangExample$stateSpace$plotBaseTemperature()



