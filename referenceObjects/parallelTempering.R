
parallelTemperingSimulation <- setRefClass(
	Class		= "ParallelTempering",
	contains	= "Simulations",

###########################################################################
								# Fields

	fields		= list(
									# User provided		

			## Temperature levels for the parallel tempering algorithm.
		temperatures		= "numeric",

			## 1/TemperatureLevel
		inverseTemperatures = "numeric",

			## Number of swapping states strategy.
		strategyNumber 		= "integer",

			## Number of temperature levels.						
		noOfTemperatures	= "integer",	

			## Matrix with two rows with pairs of possible pairs in columns.
		translatorFromLexicOrderToTranspositions = "matrix",

			## Vector with probabilities of current pair swaps.
		currentPairSwapsProbabilities 			= "numeric",
		#Current_Unnormalised_Probabilities_of_Pair_Swaps 
	
			## Vector with logs of unnormalised densities evaluated in current states.
		currentStatesLogUnnormalisedDensities	= "matrix"
	),

###########################################################################
								# Methods

	methods 	= list(	


		############################################################
				# Initialisation
				

		parallelTemperingInitializator	= function(
			temperatures 		= numeric(0),
			strategyNumber		= 1L,
			problemDimension	= 0L,
			targetDensity 		= function(){}, 
			initialStates		= matrix(nrow=0, ncol=0),
			quasiMetric 		= function(){},
			...
			)
		{
			ifelse( 
				(length(temperatures) == 0), 
				{
					cat(
						'I did not receive any temperatures. I shall therefore proceed with rather arbitrary choice of 5 temperature levels 1<2<3<4<5.'
					)
	
					tmpTemp <- 1:5	# Can add here some global constant.
	
				},
				{
					if (any( temperatures <= 1 )) cat('Do you really want to cool down the distribution? That does not make sense, does it? Try avoiding such things.')
					
					tmpTemp						<- temperatures
					tmpTemp[length(tmpTemp)+1] 	<- 1
					tmpTemp	<- 
						sort(
							tmpTemp, 
							decreasing=FALSE
						) 
				}
			)

			temperatures 		<<- tmpTemp
			inverseTemperatures <<- 1/tmpTemp
			noOfTemperatures 	<<- length(tmpTemp)

						
			tmpStrategyNumber	<- as.integer(strategyNumber)

			ifelse( 
				( is.na(tmpStrategyNumber) || tmpStrategyNumber < 0 ),		 
				stop(
					"Inappropriate stregy number. Please enter an integer value."
				)
				,
				{	
					strategyNumber	<<- tmpStrategyNumber
				}
			)


			ifelse(
				(noOfTemperatures >= 1),
				{
					translatorFromLexicOrderToTranspositions <<- 
				 		generateTranslatorFromLexicalOrderToTranspositions( 
				 			1:noOfTemperatures 
				 		)
				},
				{
					translatorFromLexicOrderToTranspositions <<- 
						matrix(
							ncol=0,
							nrow=0
						)
				}
			)

			stateSpaceStructure	<<- 
				realFiniteDimensionalStateSpaceStructure$new(
					temperatures 		= .self$temperatures,
					noOfIterations 		= noOfIterations,
					noOfTemperatures	= .self$noOfTemperatures,
					problemDimension	= problemDimension,
					targetDensity 		= targetDensity,
					initialStates 		= initialStates,
					quasiMetric 		= quasiMetric,
					...  
				)
		},

		initialize 				= function(
			noOfIterations 		= 0L,
			temperatures 		= numeric(0),
			strategyNumber		= 1L,
			problemDimension	= 0L,
			targetDensity 		= function(){}, 
			initialStates		= matrix(nrow=0, ncol=0),
			quasiMetric 		= function(){},
			...
			)
		{
			simulationInitializator(
				noOfIterations 			= noOfIterations 
			)

			parallelTemperingInitializator( 
				temperatures 		= temperatures,
				strategyNumber		= strategyNumber,
				problemDimension	= problemDimension,
				targetDensity 		= targetDensity, 
				initialStates		= initialStates,
				quasiMetric 		= quasiMetric,
				...
			)
		},


		############################################################
				# Visualisation


		parallelTemperingShow	= function()
		{
			cat('\nThe Parallel Tempering inputs are here: \n')
			cat('Temperatures: ', temperatures, '\n')
			cat('Number of chains: ', noOfTemperatures, '\n')	
			cat('Chosen swap-strategy number: ', strategyNumber, '\n\n')	
					

			print( stateSpaceStructure$showState() )	
		},

	
		show	= function()
		{
			simulationShow()
			parallelTemperingShow()			
		},
	

		############################################################
				# Algorithmic Methods
					

		makeStepOfTheAlgorithm	= function()
		{
			print('I shall make it all happen for PT.')
		},

				###### swap sphere ######

		swapStrategy = function(
			chosenIndices 	,
			strategyNumber 	=1
		)	
		{
			i <- chosenIndices[1]
			j <- chosenIndices[2] 	
			s <- strategyNumber

			tmp <- currentStatesLogDensities[i] - currentStatesLogDensities[j]

			tmp <- 
				exp(
					ifelse( s==2, -tmp, -abs(tmp) )*
					ifelse( s==3|s==4 , inverseTemperatures[i] - inverseTemperatures[j], 1)*
					ifelse( s ==4,
						1/(1 + quasiMetric( 	currentStates[,i],
												currentStates[,j] ) ),
						1 
					)
				) 

			tmp <- ifelse( s==2 & tmp >1, 1, tmp )
			
			return( tmp )	
		},

		generateTranslatorFromLexicalOrderToTranspositions = function(
			inputIndices
		)
		{
			noOfIndices 	<- length(inputIndices)
			
			if ( noOfIndices > 0)
			{
				if (noOfIndices > 1)
				{
					result<-lapply( 
						1:(noOfIndices -1),
						function( number )
						{
							noOfCols	<- noOfIndices-number
							return( 
								matrix( 
									c( 
										rep.int(x=inputIndices[number], times=noOfCols), 
										inputIndices[ (number+1):noOfIndices] 
									), 
									ncol = noOfCols,
									nrow = 2,
									byrow= TRUE 
								)
							)							
						}
					)				
				}
			} else
			{	
				stop('A problem of type in generation.')
			}				
			return(	do.call(cbind, result) )
		}

####################################################################
				# Finis Structurae
	)
)

								# No adaptation for now.
parallelTemperingSimulation$lock( 
	'temperatures',
	'inverseTemperatures',
	'noOfTemperatures',
	'strategyNumber'
)