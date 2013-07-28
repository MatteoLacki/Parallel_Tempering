# source("./referenceObjects/targetMeasures.R")
# source("./referenceObjects/targetUnnormalisedDensities.R")
# source("./referenceObjects/targetLiangDensities.R")
# source("./referenceObjects/stateSpace.R")
# source("./referenceObjects/realStateSpace.R")
# source("./referenceObjects/algorithm.R")
# source("./referenceObjects/metropolisHastings.R")
# source("./referenceObjects/parallelTempering.R")

simulation <- setRefClass(
	Class		= "Simulations",

###########################################################################
								# Fields
	fields		= list(
			## The data container with methods that act on it and deliver the probabilities.
		stateSpace		= "StateSpaces",

			## Morphisms applied to the state space. 
		algorithm 		= "Algorithms",

			## Unnormalised Probabilities of the state-space: the target measure from which we want to draw samples.
		targetMeasure 	= "TargetMeasures",	

			## Save results?    
	    save 			= "logical",


	    	## Names of used objects
	    algorithmName 		= "character",
	    stateSpaceName 		= "character",
	    targetMeasureName 	= "character"
    ),

###########################################################################
								# Methods

	methods 	= list(	
		
		############################################################
				# Initialisation

		initialize = function(
			stateSpaceName		= "real tempered",
			algorithmName		= "parallel tempering",
			targetMeasureName	= "Liang-Wang",	
			example 			= FALSE,
			iterationsNo 		= NULL,
			chainsNo 			= 0L,
			spaceDim			= 0L,
			initialStates		= matrix(nrow=0, ncol=0),
			targetDensity 		= function(){}, 
			temperatures 		= numeric(0),
			strategyNo			= 1L,
			quasiMetric 		= function(){},
			proposalCovariances = matrix(ncol=0, nrow=0),
			detailedOutput		= FALSE,
			save 				= FALSE
      )
		{
			cat("Thank you for choosing our software. We wish you a pleasent day.")

			iterationsNo 	<- checkIterationsNo( iterationsNo )
      		save 			<<- save

      		stateSpaceName 		<<- stateSpaceName
      		targetMeasureName 	<<- targetMeasureName
			algorithmName 		<<- algorithmName

			if( example )
			{
				temperatures 			<- c(1, 2.8, 7.7, 21.6, 60)
				tmpProposalCovariances 	<- vector( "list", 5L )

				# for (i in 1:5 )
				#  {
				#  	tmpProposalCovariances[[i]] <- 
				#  		diag( temperatures[i]^2, nrow=2, ncol=2 ) 				
				#  }

				for (i in 1:5 )
				{
					tmpProposalCovariances[[i]] <- 
						diag( 
							ifelse(i <=3, .05, .01)*temperatures[i]^2,
							nrow=2, 
							ncol=2 
						) 				
				}

				chainsNo			<- 5L
				spaceDim			<- 2L
				targetMeasureName 	<<- 'Liang-Wang'
				algorithmName 		<<- 'parallel tempering'					
				proposalCovariances <- tmpProposalCovariances
			}  
			
			if( algorithmName == 'parallel tempering') {
				temperatures 	<- checkTemperatures( temperatures )

				if ( 
					!( stateSpaceName %in% c(
							'real tempered'
						) 
					) 
				){ 
				stop("We have not yet developped the required state-space.")
				}

			}

			switch(
				targetMeasureName,
				'Liang-Wang'={
					targetMeasure 	<<- targetLiangDensity$new()
				},
				'any density'={
					targetMeasure 	<<- targetUDensity$new(
						targetDensity 	= targetDensity
					)
				},
				stop("You must supply a target measure.")
			)


			switch(
				stateSpaceName,
				'real'	= 
				{
					stateSpace <<- realStateSpace$new(
						iterationsNo 		= iterationsNo,
						chainsNo 			= chainsNo,
						spaceDim			= spaceDim,
						initialStates		= initialStates,
						proposalCovariances = proposalCovariances
					)
				},	
				'real tempered'	= 
				{
					stateSpace <<- realTemperedStateSpace$new(
						iterationsNo 		= iterationsNo,
						temperatures 		= temperatures,
						chainsNo 			= chainsNo,
						spaceDim			= spaceDim,
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
				'Metropolis-Hastings' 	=
				{
					algorithm <<- metropolisHastings$new(
						iterationsNo 	= iterationsNo,
						strategyNo		= strategyNo,
						detailedOutput	= detailedOutput,
						chainsNo 		= chainsNo
					)	
				},

				'parallel tempering'	= 
				{
					algorithm <<- parallelTempering$new(
						iterationsNo 	= iterationsNo,
						temperatures 	= temperatures,
						strategyNo		= strategyNo,
						detailedOutput	= detailedOutput,
						chainsNo 		= chainsNo
					)
				},

				cat("That kind of algorithm is currently unavailable.")
			)

			algorithm$stateSpace <<- stateSpace			
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
				temperatures <- 1:5	# Can add here some global constant.
			} else 
			{
				if (any( temperatures < 1 )) cat('Do you really want to cool down the distribution? That does not make sense, does it? Try avoiding such things.')
				
				if ( !(1 %in% temperatures) )
				{	#Adding base temperature.
					temperatures[ length(temperatures)+1 ] 	<- 1
				}

				temperatures	<- 
					sort(
						temperatures, 
						decreasing=FALSE
					) 
			}

			return( temperatures )
		},	


		checkIterationsNo = function( iterationsNo )
		{
			if ( !is.null(iterationsNo) ){	
				iterationsNo 	<- as.integer( iterationsNo )
		
				if ( is.na( iterationsNo ) || ( iterationsNo < 0) ) stop("Inappropriate no of steps. Please enter an integer value.")	
			}	
			
			return( iterationsNo )
		},

		############################################################
				# Visualisation


		show = function()
		{
			stateSpace$show( 
				algorithmName = algorithmName 
			)
		},

		############################################################
				# Algorithmic Methods


		simulate = function()
		{
			algorithm$simulate()

			if( !file.exists("./data") ) dir.create("./data")

			if( save ){
        		write.csv2(
	  				stateSpace$simulatedStates,
	  				file = paste(path,"simulatedStates",Sys.time(),".csv", sep="",colapse=""),
	  				row.names=FALSE
	  			)	
			}  
		}

###########################################################################
				# Finis Structurae	
	)
)

	# This will lock all fields. We want that?!
#simulation$lock( names( simulation$fields() ) )