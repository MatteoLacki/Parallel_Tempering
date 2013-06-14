rm( list = ls())
directory <- "/home/matteo/Documents/Scienza/Laurea_di_Matematica/Implementation"
setwd(directory)
source("./referenceObjects/simulations.R")
source("./referenceObjects/parallelTempering.R")


########################################################### TESTS

source("./Distributions_to_check/Liang_Example.R")
ls()

LiangWangExample <- ParallelTempering$new(
	noOfIterations	= 10,
	temperatures 	= c(2.8, 7.7, 21.6, 60),	
	strategyNumber  = 2,
	problemDimension= 2,
	targetDensity	= LIANG_TARGET_DENSITY,
	detailedOutput	= FALSE,
	proposalCovariances = list(matrix(c(4,0,0,2), ncol=2,nrow=2),matrix(c(4,0,0,2), ncol=2,nrow=2),matrix(c(100,0,0,2), ncol=2,nrow=2),matrix(c(4,0,0,2), ncol=2,nrow=2),matrix(c(6,0,0,2), ncol=2,nrow=2))
)

LiangWangExample$simulate()
LiangWangExample$transpositionHistory

LiangWangExample$stateSpace$plotBaseTemperature()
