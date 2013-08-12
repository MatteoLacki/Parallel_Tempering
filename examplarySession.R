############################### Loading files ################################
rm( list = ls())
directory <- "/home/matteo/Documents/Scienza/Laurea_di_Matematica/Implementation"
setwd(directory)

source("./targetMeasures/targetMeasures.R")
source("./targetMeasures/targetUnnormalisedDensities.R")
source("./targetMeasures/targetLiangDensities.R")
source("./targetMeasures/targetMatteoDensities.R")
source("./stateSpaces/stateSpaces.R")
source("./stateSpaces/realStateSpaces.R")
source("./stateSpaces/realTemperedStateSpaces.R")
source("./algorithms/algorithms.R")
source("./algorithms/metropolisHastings.R")
source("./algorithms/parallelTemperings.R")
source("./simulations/simulations.R")
source("./controllers/controllers.R")

############################### State-dependent simulation ###################



LiangWangExample <- simulation$new(
	iterationsNo	= 1000,
	strategyNo 	= 1,
	example 	= TRUE,
	save		= TRUE
)

LiangWangExample
system.time(
  LiangWangExample$simulate()  
) 
#LiangWangExample
LiangWangExample$stateSpace$initializeEcdfData()
LiangWangExample$stateSpace$ecdfData
system.time(
	LiangWangExample$stateSpace$kolmogorovSmirnov()
)
KS <- LiangWangExample$stateSpace$KS
KS
LiangWangExample

W <- LiangWangExample$stateSpace$ecdf
K <- LiangWangExample$stateSpace$ecdfData

dim(K)
dim(W)

maxi <- LiangWangExample$stateSpace$maximiser

LiangWangExample$targetMeasure$distribuant(maxi)
j <- K[K[,1]==maxi[1]&K[,2]==maxi[2],4]


i <- 1:nrow(K)
i <- i[K[,1]==maxi[1]&K[,2]==maxi[2]]

abs(LiangWangExample$stateSpace$ecdf[i,j] - LiangWangExample$targetMeasure$distribuant(maxi))
KS


K
M <- LiangWangExample$stateSpace$dataForPlot
head(M)
M <- M[M$Temperature==1&M$Phase=='Swap',]
dim(M)
dim(K)
M[M$x==K[367,1]&M$y==K[367,2],]
abs(-1)
W <- LiangWangExample$stateSpace$ecdf
K
tail(W)
W[40,1] 
W[40,2]
t(W[39:40,1:40])
dim(W)
head(W)
tail(W)

LiangWangExample
LiangWangExample$targetMeasure$distribuant(c(0,0))
LiangWangExample$targetMeasure$distribuant(c(10,10))

K[39,'No'] == 19
W[39,19]+K[39,'charge']
K[39,'No'] == 20


Z <- LiangWangExample$stateSpace$ecdfData
nrow(Z)
Z <- as.matrix(Z)
Z
Z[1,]


LiangWangExample$stateSpace$simulatedStates
head(LiangWangExample$stateSpace$dataForPlot)



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

############################### New Measure ############################
source("./targetMeasures/targetMatteoDensities.R")
LiangWangExample <- simulation$new(
	iterationsNo	= 10000,
	strategyNo 	= 1,
	example 	= TRUE,
	targetMeasureName = 'Matteo'	
)

LiangWangExample
system.time(
  LiangWangExample$simulate()  
) 
LiangWangExample	
LiangWangExample$stateSpace$initializeEcdfData()
Z <- LiangWangExample$stateSpace$ecdfData
#length(Z$x)
anyDuplicated(c(Z$x,Z$y))
anyDuplicated(Z$x)
anyDuplicated(Z$y)
# For 36000 different points with calculated charges: no difference whatsoever. So why to waste money ?

################################################
LiangWangExample <- simulation$new(
	iterationsNo	= 1000,
	strategyNo 	= 2,
	example 	= TRUE,
	targetMeasureName = 'Matteo'	
)

LiangWangExample
system.time(
  LiangWangExample$simulate()  
) 
LiangWangExample	

svg("MatteoDistributionStrategy2.svg", width=12, height=8)
	LiangWangExample$stateSpace$plotBaseTemperature()	
dev.off()




################################################
LiangWangExample <- simulation$new(
	iterationsNo	= 1000,
	strategyNo 	= 6,
	example 	= TRUE,
	targetMeasureName = 'Matteo'	
)

LiangWangExample
system.time(
  LiangWangExample$simulate()  
) 
LiangWangExample	

svg("MatteoDistributionStrategyUniformDistributionOnNeighbouringTranspositions.svg", width=12, height=8)
	LiangWangExample$stateSpace$plotBaseTemperature()	
dev.off()


################################################
LiangWangExample <- simulation$new(
	iterationsNo	= 1000,
	strategyNo 	= 5,
	example 	= TRUE,
	targetMeasureName = 'Matteo'	
)

LiangWangExample
system.time(
  LiangWangExample$simulate()  
) 
LiangWangExample	

svg("MatteoDistributionStrategyUniformDistributionOnAllTranspositions.svg", width=12, height=8)
	LiangWangExample$stateSpace$plotBaseTemperature()	
dev.off()

################################################


kwaziNazi <- function(x,y)
{
	return( crossprod(x-y) )
}

LiangWangExample <- simulation$new(
	iterationsNo	= 1000,
	strategyNo 	= 4,
	example 	= TRUE,
	targetMeasureName = 'Matteo',
	quasiMetric	= kwaziNazi
)

LiangWangExample
system.time(
  LiangWangExample$simulate()  
) 
LiangWangExample	


svg("MatteoDistributionStrategyWithQuasiMetric.svg", width=12, height=8)
	LiangWangExample$stateSpace$plotBaseTemperature()	
dev.off()


svg("MatteoDistributionStrategyWithQuasiMetricTranspositionHistory.svg", width=12, height=8)
	LiangWangExample$algorithm$plotHistory()
dev.off()

################################################

tmpProposalCovariances 	<- vector( "list", 5L )

for (i in 1:5 )
{
	tmpProposalCovariances[[i]] <- 
		diag( 
			ifelse(i <=3, .05, .01)*1,
			nrow=2, 
			ncol=2 
		) 				
}

LiangWangExample <- simulation$new(
	iterationsNo	= 1000,
	strategyNo 	= 6,
	algo 	 	= 'Metropolis-Hastings',
	space 	 	= 'real',
	target 		= 'Liang-Wang',
	covariances	= tmpProposalCovariances,
	spaceDim	= 2,
	chainsNo 	= 5	 
)

LiangWangExample <- Metro(
	n		= 1000,
	strategyNo 	= 6,
	space 	 	= 'real',
	target 		= 'Liang-Wang',
	covariances	= tmpProposalCovariances,
	spaceDim	= 2,
	chainsNo 	= 5	 
)

LiangWangExample <- PT(
	n		= 1000,
	strategyNo 	= 6,
	example	 	= TRUE
)

LiangWangExample
system.time(
  LiangWangExample$simulate()  
) 
LiangWangExample	

