rm( list = ls())
directory <- "/home/matteo/Documents/Scienza/Laurea_di_Matematica/Implementation"
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
f <- function( x ){ return( c( x, x^2, x[1]*x[2]) )}	

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

LiangWangExample$targetMeasure$getFirstAndSecondMoments()
LiangWangExample$targetMeasure$mixturesMeans
Matteo$targetMeasure$mixturesMeans

system.time(
  LiangWangExample$simulate()  
) 
LiangWangExample

LiangWangExample$algorithm$transpositionsHistory
X <- LiangWangExample$algorithm$swapHistory()
X[1:10]
X[11:20]
		# FIX THIS!
#LiangWangExample$targetMeasure$plotDistribuant()
#head(LiangWangExample$targetMeasure$realDensityValues)

tmpProposalCovariances 	<- vector( "list", 5L )
temperatures 	<-  c(1, 2.8, 7.7, 21.6, 60)
for (i in 1:5 )
{
	tmpProposalCovariances[[i]] <- 
		diag( 
			ifelse(i <=3, 2, .01)*temperatures[i]^2,
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

Matteo
Matteo$targetMeasure$getFirstAndSecondMoments()
Matteo$targetMeasure$sigma2
