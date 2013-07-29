Metro <- function(
	n 					= NULL,
	space 	 			= "real tempered",
	target 		 		= "Liang-Wang",	
	example 			= FALSE,
	chainsNo 			= 0L,
	spaceDim			= 0L,
	initialStates		= matrix(nrow=0, ncol=0),
	targetDensity 		= function(){}, 
	temperatures 		= numeric(0),
	strategyNo			= 1L,
	quasiMetric 		= function(){},
	covariances 		= matrix(ncol=0, nrow=0),
	detailedOutput		= FALSE,
	save 				= FALSE,
	...
)
{
	return( 
		simulation$new(
			space 	 			= space,
			algo				= 'Metropolis-Hastings',
			target 		 		= target,	
			example 			= example,
			iterationsNo 		= n,
			chainsNo 			= chainsNo,
			spaceDim			= spaceDim,
			initialStates		= initialStates,
			targetDensity 		= targetDensity, 
			temperatures 		= temperatures,
			strategyNo			= strategyNo,
			quasiMetric 		= quasiMetric,
			covariances 		= covariances,
			detailedOutput		= detailedOutput,
			save 				= save,
			...
		)
	)
}

PT <- function(
	n 					= NULL,
	space 	 			= "real tempered",
	target 		 		= "Liang-Wang",	
	example 			= FALSE,
	chainsNo 			= 0L,
	spaceDim			= 0L,
	initialStates		= matrix(nrow=0, ncol=0),
	targetDensity 		= function(){}, 
	temperatures 		= numeric(0),
	strategyNo			= 1L,
	quasiMetric 		= function(){},
	covariances 		= matrix(ncol=0, nrow=0),
	detailedOutput		= FALSE,
	save 				= FALSE,
	...
)
{
	return( 
		simulation$new(
			space 	 			= space,
			algo				= 'parallel tempering',
			target 		 		= target,	
			example 			= example,
			iterationsNo 		= n,
			chainsNo 			= chainsNo,
			spaceDim			= spaceDim,
			initialStates		= initialStates,
			targetDensity 		= targetDensity, 
			temperatures 		= temperatures,
			strategyNo			= strategyNo,
			quasiMetric 		= quasiMetric,
			covariances 		= covariances,
			detailedOutput		= detailedOutput,
			save 				= save,
			...
		)
	)
}