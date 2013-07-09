############################### Loading files ################################
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

############################### State-dependent simulation ###################
LiangWangExample <- simulation$new(
	iterationsNo	= 1000,
	strategyNo 	= 1,
	example 	= TRUE	
)

LiangWangExample
system.time(
  LiangWangExample$simulate()  
) 
LiangWangExample	
LiangWangExample$algorithm$plotHistory()

############################### State-independent simulation #################


	### Uniform distribution on all transpositions.
source("./stateSpaces/realStateSpaces.R")
LiangWangExample <- simulation$new(
	iterationsNo	= 10,
	strategyNo 	= 5,
	example 	= TRUE	
)

LiangWangExample
system.time(
  LiangWangExample$simulate()  
) 
LiangWangExample	
LiangWangExample$algorithm$plotHistory()
head(LiangWangExample$targetMeasure$realDensityValues)

	### Uniform distribution on neighbouring transpositions.

LiangWangExample <- simulation$new(
	iterationsNo	= 1000,
	strategyNo 	= 6,
	example 	= TRUE	
)

LiangWangExample
system.time(
  LiangWangExample$simulate()  
) 
LiangWangExample
LiangWangExample$stateSpace$plotAllChains()	
LiangWangExample$algorithm$plotHistory()


############################### Additional Topics ############################
X <- LiangWangExample$stateSpace$dataForPlot[,1:2]

colMeans(X^2)


head(LiangWangExample$stateSpace$dataForPlot)
X <- LiangWangExample$stateSpace$dataForPlot
X <- X[X$Temperature == 1,]
X
X <- X[,1:2]

svg("histogram.svg", width=6, height=4)
	LiangWangExample$algorithm$plotHistory()
dev.off()
