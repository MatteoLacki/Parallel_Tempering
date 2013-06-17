# source("./referenceObjects/stateSpace.R")
# source("./referenceObjects/realStateSpace.R")
# source("./referenceObjects/liangRealStateSpace.R")
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
		algorithm 	= "Algorithms"	
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
			strategyNumber		= 1L,
			spaceDim			= 0L,
			targetDensity 		= function(){}, 
			initialStates		= matrix(nrow=0, ncol=0),
			quasiMetric 		= function(){},
			proposalCovariances = matrix(ncol=0, nrow=0),
			example 			= FALSE,
			detailedOutput		= FALSE
		)
		{
			temperatures 	<- checkTemperatures( temperatures )
			temperaturesNo	<- length( temperatures )

			switch(
				stateSpaceName,
				RealStateSpace	= 
				{
					stateSpace <<- realStateSpace$new(
						iterationsNo 		= iterationsNo,
						temperatures 		= temperatures,
						temperaturesNo 		= temperaturesNo,
						strategyNumber		= strategyNumber,
						spaceDim			= spaceDim,
						targetDensity 		= targetDensity, 
						initialStates		= initialStates,
						quasiMetric 		= quasiMetric,
						proposalCovariances = proposalCovariances,
						example 			= example,
						detailedOutput		= detailedOutput	
					)	
				},

				LiangRealStateSpace = 
				{
					stateSpace <<- liangRealStateSpace$new(
						iterationsNo  	= iterationsNo,
						initialStates  	= initialStates,
						quasiMetric 	= quasiMetric
					)	
				},

				cat("That kind of state-space is currently unavailable.")

			)

			switch(
				algorithmName,
				ParallelTempering	= 
				{
					algorithm <<- parallelTempering$new(
						iterationsNo 	= iterationsNo,
						temperatures 	= temperatures,
						strategyNumber	= strategyNumber,
						detailedOutput	= detailedOutput
					)

					algorithm$stateSpace <<- stateSpace
				},
				MetropolisHasting 	=
				{
					if( length(temperatures) != 1) 
						cat(" I shall proceed with a number of chains equal to the number of temperatures, although the termperature levels are irrelevant in the apllication of the MH algorithm.")	
					cat("Metropolis-Hasting currently unavailable.")
				},
			
				cat("That kind of algorithm is currently unavailable.")
			)	
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

		############################################################
				# Visualisation

		show = function()
		{

		},

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
simulation$lock( names( simulation$fields() ) )