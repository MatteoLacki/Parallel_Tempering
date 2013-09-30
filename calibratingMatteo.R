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
#ifelse(i <=3, 2, .5)*temperatures[i]^2,
weights <- c(.4,1,.5,.18,.05)
for (i in 1:5 )
{
	tmpProposalCovariances[[i]] <- 
		diag( 
			weights[i]*temperatures[i]^2,			
			nrow=2, 
			ncol=2 
		) 				
}
tmpProposalCovariances

f <- function( x ){ return( c( x, x^2, x[1]*x[2]) )}	

euclid <- function(x,y)
{
	return( crossprod(x-y) )
}

Matteo <- simulation$new(
	target 		= 'Matteo',
	iterationsNo	= 14000,
	strategyNo 	= 2,
	burnIn 		= 2000,
	chainsNo 	= 5,
	spaceDim 	= 2,
	temperatures 	= temperatures,	
	covariances 	= tmpProposalCovariances,
	quasiMetric 	= euclid,
	trialNo 	= 1L,
	initialStates 	= matrix(rep.int(8, times=10) ,nrow=2, ncol=5, byrow=TRUE),
	evaluateKS 	= FALSE,
	integratedFunction = f,
	rememberStates  = FALSE,
	evaluateSojourn = TRUE
)

Matteo$simulate()
Matteo
Matteo$targetMeasure$getFirstAndSecondMoments()
Matteo$targetMeasure$sojournTimes

Matteo$stateSpace$plotAllTemperatures()
