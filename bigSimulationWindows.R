rm( list = ls())
#directory <- "/home/matteo/Documents/Scienza/Laurea_di_Matematica/Implementation"
directory <- "F:/Mateusz/gitHub/Parallel_Tempering"
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


BigSimulaton <- function( trialNo, minStrat, maxStrat )
{
  euclid <- function(x,y)
  {
  	return( crossprod(x-y) )
  }
  
  strategyNo  <- maxStrat - minStrat + 1
  
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
  
  
    for( strategy in minStrat:maxStrat ){
    	for( trial in 1:trialNo ){
          LiangWangExample <- simulation$new(
      			iterationsNo	= 7500,
      			strategyNo 	= strategy,
      			example 	= TRUE,
      			burnIn 		= 2500,
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
  
  return( results )
}

write.csv2(
	BigSimulaton( 200, 1, 3),
	file = paste(
	directory,
	"/bigSimulations/bigSimulationStrat1to3with200trials.csv",
	sep="",
	collapse=""
	),
	row.names=FALSE
)	

