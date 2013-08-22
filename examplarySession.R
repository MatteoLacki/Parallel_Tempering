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

euclid <- function(x,y)
{
	return( crossprod(x-y) )
}

trialNo 	<- 10L
strategyNo	<- 6L

results <- as.data.frame(matrix(nrow=trialNo*strategyNo,ncol=63))

nameCreator <- function( letter, minNo, maxNo)	
{
	sapply(
		minNo:maxNo,
		function(num){
			return( paste( letter, num, sep="",collapse="" ) )
		}
	)	
}
	
naming  <- c(
	'Strategy',
	nameCreator('rwByTemp',1,5), 
	nameCreator('rswap',0,10),
	'KS',
	nameCreator('MeanNo',1,20),
	nameCreator('MeanNo',1,20),
	'EX',
	'EY',
	'EX2',
	'EY2',
	'EXY'
) 	

i <- 1L	
for( strategy in 1:strategyNo ){
	for( trial in 1:trialNo ){
		LiangWangExample <- simulation$new(
			iterationsNo	= 100,
			strategyNo 	= strategy,
			example 	= TRUE,
			burnIn 		= 200,
			save		= FALSE,
			trialNo 	= trial,
			quasiMetric 	= euclid,
			evaluateKS 	= TRUE
		)
		LiangWangExample$simulate()
		results[i,] <- LiangWangExample$furnishResults()
		i <- i+1
		rm(LiangWangExample)	
	}
}

names( results ) <- naming
results

############################### Additional Topics ############################

LiangWangExample <- simulation$new(
	iterationsNo	= 75,
	strategyNo 	= 1,
	example 	= TRUE,
	burnIn 		= 25,
	save		= TRUE,
	trialNo 	= 1L,
	evaluateKS 	= TRUE
)

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
LiangWangExample$stateSpace$initializeEcdfData()
system.time(
  LiangWangExample$stateSpace$kolmogorovSmirnov(resolution=0)
)
KS <- LiangWangExample$stateSpace$KS
KS

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

