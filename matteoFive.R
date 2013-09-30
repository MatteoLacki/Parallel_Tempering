rm( list = ls())
#directory <- "/home/matteo/Documents/Scienza/Laurea_di_Matematica/Implementation"
#directory <- "F:/Mateusz/gitHub/Parallel_Tempering"
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
		nameCreator('randomWalkAcceptances',1,5), 
		nameCreator('randomSwapAccepted',1,10),
		nameCreator('randomSwaps',1,10),
		'KS',
		nameCreator('euclideanClusterMean',1,2),
		nameCreator('chiSquareClusterMean',1,2),
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

	proposalCovariances 	<- vector( "list", 5L )
	temperatures 	<- c(1, 2.8, 7.7, 21.6, 60)
	weights		<- c(3,1,.5,.18,.05)
	for (i in 1:5 )
	{
		proposalCovariances[[i]] <- 
			diag( 
				weights[i]*temperatures[i]^2,
				nrow=2, 
				ncol=2 
			) 				
	}


	
	i <- 1L 
	for( strategy in minStrat:maxStrat ){
		for( trial in 1:trialNo ){
			
			cat('Strategy',strategy,'\n')
			cat('Trial',trial,'\n')
			
			Matteo <- simulation$new(
				target 		= 'Matteo',
				iterationsNo	= 7500,
				strategyNo 	= strategy,
				burnIn 		= 2500,
				chainsNo 	= 5,
				spaceDim 	= 2,
				temperatures 	= temperatures,	
				covariances 	= proposalCovariances,
				trialNo 	= trial,
				evaluateKS 	= TRUE,
				integratedFunction = functionToIntegrate,
				rememberStates  = TRUE,
				evaluateSojourn = TRUE,
				quasiMetric	= euclid
			)

			Matteo$simulate()
			results[i,] <- Matteo$furnishResults()

			write.csv2(
				results[1:i,],
				file =	"./bigSimulations/MatteoFive.csv",
				row.names=FALSE
			) 

			i <- i+1

			rm(Matteo)     
		}
	}
	
	return( results )
}

results <- BigSimulaton( 100, 5, 5)

write.csv2(
	results,
	file = "./bigSimulations/MatteoFive.csv",
	row.names=FALSE
)	

