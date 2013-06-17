# source("./referenceObjects/targetMeasures.R")
# source("./referenceObjects/targetUnnormalisedDensities.R")
# source("./referenceObjects/targetLiangDensities.R")
# source("./referenceObjects/stateSpace.R")
# source("./referenceObjects/realStateSpace.R")
# source("./referenceObjects/algorithm.R")
# source("./referenceObjects/parallelTempering.R")

simulation <- setRefClass(
	Class		= "Simulations",

###########################################################################
								# Fields
	fields		= list(

			## The data container with methods that act on it and deliver the probabilities.
		stateSpace	= "StateSpaces",

			## Morphisms applied to the state space. 
		algorithm 	= "Algorithms",

			## Unnormalised Probabilities of the state-space: the target measure from which we want to draw samples.
		targetMeasure 	= "TargetMeasures"	
	),

###########################################################################
								# Methods

	methods 	= list(	
		
		############################################################
				# Initialisation

		initialize = function(
			stateSpaceName		= "LiangRealStateSpace",
			algorithmName		= "ParallelTempering",
	
			iterationsNo 		= 0L,
			temperatures 		= numeric(0),
			strategyNo			= 1L,
			spaceDim			= 0L,
			targetDensity 		= function(){}, 
			initialStates		= matrix(nrow=0, ncol=0),
			quasiMetric 		= function(){},
			proposalCovariances = matrix(ncol=0, nrow=0),
			example 			= FALSE,
			detailedOutput		= FALSE
		)
		{
			print("Thank you for choosing our software. We hope that you will have a pleasent day.")

			iterationsNo 	<- checkIterationsNo( iterationsNo )

			if( example )
			{
				temperatures 			<- c(1, 2.8, 7.7, 21.6, 60)

				tmpProposalCovariances 	<- vector( "list", 5L )

				for (i in 1:5 )
				{
					tmpProposalCovariances[[i]] <- 
						diag( temperatures[i]^2, nrow=2, ncol=2 ) 				
				}

				stateSpace 	<<- 
					realStateSpace$new(
						iterationsNo 		= iterationsNo,
						temperatures 		= temperatures,
						temperaturesNo 		= 5L,
						spaceDim			= 2L,
						initialStates		= initialStates,
						quasiMetric 		= quasiMetric,
						proposalCovariances = tmpProposalCovariances
					)

				targetMeasure 	<<- targetLiangDensity$new()

				stateSpace$targetMeasure <<- targetMeasure

				algorithm <<- 
					parallelTempering$new(
						iterationsNo 	= iterationsNo,
						temperatures 	= temperatures,
						strategyNo		= strategyNo,
						detailedOutput	= detailedOutput
					)

				algorithm$stateSpace <<- stateSpace
					
			} else 
			{	
				temperatures 	<- checkTemperatures( temperatures )
				temperaturesNo	<- length( temperatures )
	

				targetMeasure 	<<- 
					targetUDensity( targetDensity = targetDensity )

				switch(
					stateSpaceName,
					RealStateSpace	= 
					{
						stateSpace <<- realStateSpace$new(
							iterationsNo 		= iterationsNo,
							temperatures 		= temperatures,
							temperaturesNo 		= temperaturesNo,
							spaceDim			= spaceDim,
							#targetDensity 		= targetDensity, 
							initialStates		= initialStates,
							quasiMetric 		= quasiMetric,
							proposalCovariances = proposalCovariances
						)	
					},
	
					cat("That kind of state-space is currently the only one unavailable.")
				)

				stateSpace$targetMeasure  <<- targetMeasure
	
				switch(
					algorithmName,
					ParallelTempering	= 
					{
						algorithm <<- parallelTempering$new(
							iterationsNo 	= iterationsNo,
							temperatures 	= temperatures,
							strategyNo	= strategyNo,
							detailedOutput	= detailedOutput
						)
					},
					MetropolisHasting 	=
					{
						if( length(temperatures) != 1) 
							cat(" I shall proceed with a number of chains equal to the number of temperatures, although the termperature levels are irrelevant in the apllication of the MH algorithm.")	
						cat("Metropolis-Hasting currently unavailable.")
					},
				
					cat("That kind of algorithm is currently unavailable.")
				)

				algorithm$stateSpace <<- stateSpace
			}	
		},


		checkTemperatures = function(
			temperatures
		)
		{
			if (length(temperatures) == 0)
			{
				cat(
					'I did not receive any temperatures. I shall therefore proceed with rather arbitrary choice of 5 temperature levels 1<2<3<4<5.'
				)
				tmpTemp <- 1:5	# Can add here some global constant.
			} else 
			{
				if (any( temperatures <= 1 )) cat('Do you really want to cool down the distribution? That does not make sense, does it? Try avoiding such things.')
				
				if ( !(1 %in% temperatures) )
				{	#Adding base temperature.
					tmpTemp						<- temperatures
					tmpTemp[length(tmpTemp)+1] 	<- 1
				}

				tmpTemp	<- 
					sort(
						tmpTemp, 
						decreasing=FALSE
					) 
			}

			return( tmpTemp )
		},	


		checkIterationsNo = function( iterationsNo )
		{
			iterationsNo 	<- as.integer( iterationsNo )
	
			if ( is.na( iterationsNo ) || ( iterationsNo < 0) ) 
			{
				stop("Inappropriate no of steps. Please enter an integer value.")
			} else
			{	
				return( iterationsNo )
			}	
		},

		############################################################
				# Visualisation

		# show = function()
		# {
		# 	stateSpace$show()
		# },

		############################################################
				# Algorithmic Methods

		simulate = function()
		{
			algorithm$simulate()			
		}

###########################################################################
				# Finis Structurae	
	)
)

	# This will lock all fields. We want that?!
#simulation$lock( names( simulation$fields() ) )