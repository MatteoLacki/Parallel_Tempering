# source("./referenceObjects/targetMeasures.R")
# source("./referenceObjects/targetUnnormalisedDensities.R")
# source("./referenceObjects/targetLiangDensities.R")
# source("./referenceObjects/targetMatteoDensities.R")
# source("./referenceObjects/stateSpace.R")
# source("./referenceObjects/realStateSpace.R")
# source("./referenceObjects/algorithm.R")
# source("./referenceObjects/metropolisHastings.R")
# source("./referenceObjects/parallelTempering.R")
# source("./functionsToIntegrate/functionsToIntegrate.R")

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

		integrant 		= "FunctionsToIntegrate",

			## Save results?    
	    save 			= "logical",

	    	## Names of used objects
	    algorithmName 		= "character",
	    stateSpaceName 		= "character",
	    targetMeasureName 	= "character",
	    strategyNo 			= "integer",
	    trialNo 			= "integer",
	    iterationsNo 		= "integer",

	    directoryToWrite 	= "character",
	    evaluateKS 			= "logical"
    ),

###########################################################################
								# Methods

	methods 	= list(	
		
		############################################################
				# Initialisation

		initialize = function(
			space 	 			= "real tempered",
			algo 				= "parallel tempering",
			target 		 		= "Liang-Wang",	
			example 			= FALSE,
			iterationsNo 		= NULL,
			burnIn				= 2000L,
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
			trialNo 			= 1L,
			evaluateKS 			= FALSE,
			integratedFunction  = function(){ return(0L)},
			rememberStates 		= FALSE,
			evaluateSojourn 	= FALSE,
			...
      )
		{
			if (!is.null(iterationsNo)){

				cat("Thank you for choosing our software. We wish you a pleasent day.")
	
				setIterationsNo( iterationsNo=iterationsNo )

	      		save 				<<- save
	      		stateSpaceName 		<<- space
	      		targetMeasureName 	<<- target
				algorithmName 		<<- algo
				strategyNo 			<<- as.integer(strategyNo)
				trialNo				<<- trialNo
				evaluateKS 			<<- evaluateKS
				proposalCovariances <-  covariances 

				if ( !rememberStates ){
					evaluateKS 			<<- evaluateKS
				}

				if( as.integer(burnIn) >= 0 )
				{
					burnIn <- as.integer(burnIn)
				}

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
						targetMeasure 	<<- targetLiangDensity$new(iterationsNo = iterationsNo)
					},
					'Matteo'={
						targetMeasure 	<<- targetMatteoDensity$new(iterationsNo = iterationsNo)
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
							chainsNo 			= as.integer(chainsNo),
							spaceDim			= spaceDim,
							initialStates		= initialStates,
							proposalCovariances = proposalCovariances,
							rememberStates 		= rememberStates,
							evaluateSojourn 	= evaluateSojourn	
						)
					},	
					'real tempered'	= 
					{
						stateSpace <<- realTemperedStateSpace$new(
							iterationsNo 		= iterationsNo,
							temperatures 		= temperatures,
							chainsNo 			= as.integer(chainsNo),
							spaceDim			= spaceDim,
							initialStates		= initialStates,
							quasiMetric 		= quasiMetric,
							proposalCovariances = proposalCovariances,
							rememberStates 		= rememberStates,
							evaluateSojourn 	= evaluateSojourn
						)	
					},
					cat("That kind of state-space is currently the only one unavailable.")
				)
	
				stateSpace$targetMeasure  	<<- targetMeasure

				integrant 	<<- functionToIntegrate$new(
					integratedFunction = integratedFunction
				)

				stateSpace$integrant  		<<- integrant
	
				switch(
					algorithmName,
					'Metropolis-Hastings' 	=
					{
						algorithm <<- metropolisHastings$new(
							iterationsNo 	= iterationsNo,
							burnIn 			= burnIn,
							strategyNo		= strategyNo,
							detailedOutput	= detailedOutput,
							chainsNo 		= as.integer(chainsNo)
						)	
					},
					'parallel tempering'	= 
					{
						algorithm <<- parallelTempering$new(
							iterationsNo 	= iterationsNo,
							burnIn 			= burnIn,
							temperatures 	= temperatures,
							strategyNo		= strategyNo,
							detailedOutput	= detailedOutput,
							chainsNo 		= as.integer(chainsNo)
						)
					},
	
					cat("That kind of algorithm is currently unavailable.")
				)
	
				algorithm$stateSpace <<- stateSpace			}
		},


		checkTemperatures = function(
			temperatures
		){
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
	
		setIterationsNo = function( iterationsNo )
		{
			if ( is.na( iterationsNo ) || ( iterationsNo < 0) ) 
			{
				stop("Inappropriate no of steps. Please enter an integer value.")	
			} else {	
				iterationsNo <<- as.integer( iterationsNo )
			}
		},


		############################################################
				# Visualisation and Saving


		show = function()
		{
			algorithm$show()
			stateSpace$show( 
				algorithmName = algorithmName 
			)
			targetMeasure$show()
			integrant$show()
		},


		write = function() 
		{
			
			if( !file.exists("./data") ) dir.create("./data")

			parallel <- algorithmName=='parallel tempering'

			directoryToWrite <<- paste(
				getwd(),
				"/data/",
				algorithmName,
				'[]',
				stateSpaceName,
				'[]',
				targetMeasureName,
				ifelse( 
					parallel,
					paste(
						'[]strat',
						strategyNo,
						'[]',
						sep="",
						collapse=""
					),
					'[]'
				),
				iterationsNo,
				"iter",
				'[]trial', 
				trialNo,
				sep="",
				colapse=""
			)

			if( !file.exists( directoryToWrite ) ) dir.create( directoryToWrite )

			stateSpace$writeStates( directoryToWrite )
  			algorithm$writeInfo( directoryToWrite )
	  		stateSpace$writeSojourns( directoryToWrite )

  			if( evaluateKS ){				  
  				stateSpace$writeKS( directoryToWrite )
  			}

  			if( parallel ){
  				algorithm$writeSwaps( directoryToWrite )
  			}

		},


		furnishResults = function(){
			
			spaceDim  		<- stateSpace$spaceDim 

			results  <- list( strategyNo=strategyNo )

			if( stateSpaceName=='real tempered' ){
				results$randomWalksRejections  	<- 
					algorithm$randomWalkHistory()
			} 

			if( algorithmName=='parallel tempering' ){
				results$transpositionRejections  <- 
					algorithm$swapHistory()
			}			

			if( evaluateKS ){
				results$KS <- stateSpace$KS			
			} else {
				results$KS <- 2
			}

			sojournEstimates 	<- targetMeasure$sojournTimes 

			results$euclideanClassifier <- 
				sojournEstimates[1,]/iterationsNo

			results$chiSquareClassifier <- 
				sojournEstimates[2,]/iterationsNo

			results$integralEstimates 	<- 
				integrant$approximation

			return(
				Reduce(
					c, 
					results
				)
			)	
		},


		############################################################
				# Algorithmic Methods


		simulate = function()
		{
			algorithm$simulate()

			if( evaluateKS ){
				stateSpace$kolmogorovSmirnov( resolution=0 )
			}	

			if( save ) write()
		}



###########################################################################
				# Finis Structurae	
	)
)

	# This will lock all fields. We want that?!
#simulation$lock( names( simulation$fields() ) )