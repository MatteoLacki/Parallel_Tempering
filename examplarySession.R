############################### Loading files ################################
rm( list = ls())
#directory <- "/home/matteo/Documents/Scienza/Laurea_di_Matematica/Implementation"
directory <- "/home/matteo/Documents/Scienza/scientificProjects/Parallel_Tempering"
setwd(directory)

source("./targetMeasures/targetMeasures.R")
source("./targetMeasures/targetUnnormalisedDensities.R")
source("./targetMeasures/targetLiangDensities.R")
source("./targetMeasures/targetMatteoDensities.R")
source("./functionsToIntegrate/functionsToIntegrate.R")
source("./stateSpaces/stateSpaces.R")
source("./stateSpaces/realStateSpaces.R")
source("./stateSpaces/realTemperedStateSpaces.R")
source("./algorithms/algorithms.R")
source("./algorithms/metropolisHastings.R")
source("./algorithms/parallelTemperings.R")
source("./simulations/simulations.R")
source("./controllers/controllers.R")

############################### State-dependent simulation ###################


BigSimulaton <- function( trialNo, minStrat, maxStrat )
{
  euclid <- function(x,y)
  {
  	return( crossprod(x-y) )
  }
  
  strategyNo  <- maxStrat - minStrat + 1
  
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
  	nameCreator('randomWalkTemperature',1,5), 
  	nameCreator('randomSwaps',1,10),
  	nameCreator('acceptedRandomSwaps',1,10),
  	'KS',
  	nameCreator('euclideanClusterMean',1,20),
  	nameCreator('chiSquareClusterMean',1,20),
  	'EX',
  	'EY',
  	'EX2',
  	'EY2',
  	'EXY'
  ) 	
  
  	results <- as.data.frame(
  		matrix(
  			nrow=trialNo*strategyNo,
  			ncol=length(naming)
  		)
  	)

  	names( results ) <- naming
  
  	functionToIntegrate <- function( x ){ return( c( x, x^2, x[1]*x[2]) )}	

 	i <- 1L 
    for( strategy in minStrat:maxStrat ){
    	for( trial in 1:trialNo ){
          LiangWangExample <- simulation$new(
				iterationsNo	= 75,
				strategyNo 	= strategy,
				example 	= TRUE,
				burnIn 		= 25,
				save		= FALSE,
				trialNo 	= trial,
				evaluateKS 	= FALSE,
				integratedFunction = functionToIntegrate,
				rememberStates  = FALSE,
				evaluateSojourn = TRUE,
				quasiMetric	= euclid
      		)
      		LiangWangExample$simulate()
      		results[i,] <- LiangWangExample$furnishResults()

      		write.csv2(
				results[1:i,],
				file = paste(
				directory,
				"/bigSimulations/partialResults.csv",
				sep="",
				collapse=""
				),
				row.names=FALSE
			)	
      		
      		i <- i+1

      		rm(LiangWangExample)		 
    	}
    }
  
  
  
  return( results )
}

x <- BigSimulaton( 1, 4, 6)

############################### Additional Topics ############################
rm( list = ls())
#directory <- "/home/matteo/Documents/Scienza/Laurea_di_Matematica/Implementation"
#setwd(directory)

source("./targetMeasures/targetMeasures.R")
source("./targetMeasures/targetUnnormalisedDensities.R")
source("./targetMeasures/targetLiangDensities.R")
source("./targetMeasures/targetMatteoDensities.R")
source("./functionsToIntegrate/functionsToIntegrate.R")
source("./stateSpaces/stateSpaces.R")
source("./stateSpaces/realStateSpaces.R")
source("./stateSpaces/realTemperedStateSpaces.R")
source("./algorithms/algorithms.R")
source("./algorithms/metropolisHastings.R")
source("./algorithms/parallelTemperings.R")
source("./simulations/simulations.R")
source("./controllers/controllers.R")


f <- function( x ){ return( c( x, x^2, x[1]*x[2]) )}

temperatures 			<- c(1, 2.8, 7.7, 21.6, 60)
tmpProposalCovariances 	<- vector( "list", 5L )

LiangWangExample <- simulation$new(
	iterationsNo	= 75,
	strategyNo 	= 7,
	example 	= TRUE,
	burnIn 		= 25,
	trialNo 	= 1L,
	evaluateKS 	= TRUE,
	integratedFunction = f,
	rememberStates  = TRUE,
	evaluateSojourn = TRUE
)
LiangWangExample$targetMeasure$getFirstAndSecondMoments()

system.time(
  LiangWangExample$simulate()  
) 
LiangWangExample$furnishResults()
LiangWangExample$algorithm$plotHistory()

LiangWangExample$integrant$approximation

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

#################################################

rm( list = ls())
#directory <- "/home/matteo/Documents/Scienza/Laurea_di_Matematica/Implementation"
#setwd(directory)
source("./targetMeasures/targetMeasures.R")
source("./targetMeasures/targetUnnormalisedDensities.R")
source("./targetMeasures/targetLiangDensities.R")
source("./targetMeasures/targetMatteoDensities.R")
source("./functionsToIntegrate/functionsToIntegrate.R")
source("./stateSpaces/stateSpaces.R")
source("./stateSpaces/realStateSpaces.R")
source("./stateSpaces/realTemperedStateSpaces.R")
source("./algorithms/algorithms.R")
source("./algorithms/metropolisHastings.R")
source("./algorithms/parallelTemperings.R")
source("./simulations/simulations.R")
source("./controllers/controllers.R")

f <- function( x ){ return( c( x, x^2, x[1]*x[2]) )}	

tmpProposalCovariances 	<- vector( "list", 5L )
temperatures 	<-  c(1, 2.8, 7.7, 21.6, 60)
for (i in 1:5 )
{
	tmpProposalCovariances[[i]] <- 
		diag( 
			ifelse(i <=3, .05, .01)*temperatures[i]^2,
			nrow=2, 
			ncol=2 
		) 				
}

Matteo <- simulation$new(
	target 		= 'Matteo',
	iterationsNo	= 75,
	strategyNo 	= 2,
	burnIn 		= 25,
	chainsNo 	= 5,
	spaceDim 	= 2,
	temperatures 	= c(1, 2.8, 7.7, 21.6, 60),	
	covariances 	= tmpProposalCovariances,
	trialNo 	= 1L,
	evaluateKS 	= TRUE,
	integratedFunction = f,
	rememberStates  = TRUE,
	evaluateSojourn = TRUE
)

system.time(
  Matteo$simulate()  
) 

####################################################################
tmpProposalCovariances 	<- vector( "list", 5L )
temperatures 	<-  c(1, 2.8, 7.7, 21.6, 60)
for (i in 1:5 )
{
	tmpProposalCovariances[[i]] <- 
		diag( 
			ifelse(i <=3, .05, .01)*temperatures[i]^2,
			nrow=2, 
			ncol=2 
		) 				
}

LiangWangExample <- simulation$new(
	target 		= 'Liang-Wang',
	iterationsNo	= 75,
	strategyNo 	= 2,
	burnIn 		= 25,
	chainsNo 	= 5,
	spaceDim 	= 2,
	temperatures 	= c(1, 2.8, 7.7, 21.6, 60),	
	covariances 	= tmpProposalCovariances,
	trialNo 	= 1L,
	evaluateKS 	= TRUE,
	integratedFunction = f,
	rememberStates  = TRUE,
	evaluateSojourn = TRUE
)

system.time(
  LiangWangExample$simulate()  
) 

