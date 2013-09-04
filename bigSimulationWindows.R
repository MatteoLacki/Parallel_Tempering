rm( list = ls())
directory <- "/home/matteo/Documents/Scienza/Laurea_di_Matematica/Implementation"
#directory <- "F:/Mateusz/gitHub/Parallel_Tempering"
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
				iterationsNo= 1000,
				strategyNo  = strategy,
				example   = TRUE,
				burnIn    = 250,
				save    = FALSE,
				trialNo   = trial,
				evaluateKS  = FALSE,
				integratedFunction = functionToIntegrate,
				rememberStates  = FALSE,
				evaluateSojourn = TRUE
			)

			LiangWangExample$simulate()
			results[i,] <- LiangWangExample$furnishResults()

			write.csv2(
				results[1:i,],
				file = paste(
					directory,
					"/MakingSimulations/partialResults.csv",
					sep="",
					collapse=""
				),
				row.names=FALSE,
				append=TRUE
			) 

			i <- i+1

			rm(LiangWangExample)     
		}
	}
	
	return( results )
}

results <- BigSimulaton( 1, 1, 3)

write.csv2(
	results,
	file = paste(
		directory,
		"/MakingSimulations/trial.csv",
		sep="",
		collapse=""
	),
	row.names=FALSE
)	

