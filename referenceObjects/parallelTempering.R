
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

			## Number of transpositions in the lexicographic order.
		noOfTranspositions	= "integer",

			## Vector with probabilities of current pair swaps.
		lastSwapUProbabilities	= "numeric",
	
			## Vector with probabilities of current pair swaps.
		lastProposalSwapProbabilities	= "numeric",

			## Vector with logs of unnormalised densities evaluated at current states. They are not yet multiplied by the inverse temperatures: this vector does not correspond to evaluation of the tensorised target measure.
		lastStatesLogUnDensities	= "numeric",

			## Vector with logs of unnormalised densities evaluated at current proposal. They are not yet multiplied by the inverse temperatures: this vector does not correspond to evaluation of the tensorised target measure. 

			#This can be turned into a local variable.
		lastProposalLogUnDensities	= "numeric",

			## Vector with boolean values. TRUE when a state did get updated in the Random Walk phase of the algorithm.
		statesUpdatedByRandomWalk	= "logical",	

			## An integer value describing the number of swap in the lexicographical order. -1 corresponds to swap rejection.
		noOfLastTransposition	= "integer",

			## A vector of integers : all transpositions enlisted. As long as the provided number of iterations to be executed.
		transpositionHistory	= "integer",

			## Triggers detailed output during the simulation.
		detailedOutput		= "logical"
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
			proposalCovariances = matrix(ncol=0, nrow=0),
			detailedOutput		= FALSE
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

				# MAYBE UNNECESARY
			ifelse(
				(noOfTemperatures >= 1),
				{
					translatorFromLexicOrderToTranspositions <<- 
				 		generateTranspositions( 
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

			#noOfTranspositions <<- noOfTemperatures*(noOfTemperatures-1)/2
			noOfTranspositions 	<<- 
				length( translatorFromLexicOrderToTranspositions )

			stateSpace	<<- 
				realFiniteDimensionalstateSpace$new(
					temperatures 		= .self$temperatures,
					noOfIterations 		= noOfIterations,
					noOfTemperatures	= .self$noOfTemperatures,
					problemDimension	= problemDimension,
					targetDensity 		= targetDensity,
					initialStates 		= initialStates,
					quasiMetric 		= quasiMetric,
					proposalCovariances = proposalCovariances
				)

				# Initially everything is new.
			statesUpdatedByRandomWalk 		<<- rep( TRUE, noOfTemperatures)

				# Current states must get at least once calculated all without any updates.
			lastStatesLogUnDensities 	<<-  
				stateSpace$getProposalLogsOfUnnormalisedDensities()

			noOfLastTransposition <<- -1L
			transpositionHistory	<<-	integer(noOfIterations)

			detailedOutput		<<- detailedOutput
		},

		initialize 				= function(
			noOfIterations 		= 0L,
			temperatures 		= numeric(0),
			strategyNumber		= 1L,
			problemDimension	= 0L,
			targetDensity 		= function(){}, 
			initialStates		= matrix(nrow=0, ncol=0),
			quasiMetric 		= function(){},
			proposalCovariances = matrix(ncol=0, nrow=0),
			detailedOutput		= FALSE
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
				proposalCovariances = proposalCovariances,
				detailedOutput		= detailedOutput
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
					

			print( stateSpace$showState() )	
		},

	
		show	= function()
		{
			simulationShow()
			parallelTemperingShow()			
		},
	

		############################################################
				# Algorithmic Methods
					

		makeStepOfTheAlgorithm	= function( 
			iteration 
		)
		{
			if( detailedOutput ) 
				cat("===============================================================\n",
				"Random Walk No ", 	iteration,	'\n')
			
			randomWalk()

			if( detailedOutput ) 
				cat("===============================================================\n",
				"Swap Step No ", 	iteration,	'\n')

			swap()	

		},


				###### random walk sphere ######

		randomWalk = function()
		{
			lastProposalLogUnDensities <<- 		
				stateSpace$randomWalkProposal()

			if ( detailedOutput ) 
				cat(
					"\nPrevious Log Densities:\n",
					lastStatesLogUnDensities,
					"\nProposal Log Densities:\n",
					lastProposalLogUnDensities,
					"\n"
				)
			
			rejection()
			updateAfterRandomWalk() # both logdensities and current states.
		},


		rejection = function()
		{
			Ulog <- log( runif( noOfTemperatures ) )

			toCompareWithLogU <- 
				sapply(
					1:noOfTemperatures,
					function( k )
					{
						inverseTemperatures[k]*
						(
							lastProposalLogUnDensities[k] -
							lastStatesLogUnDensities[k]
						)

					}
				)

			if ( detailedOutput ) 
				cat(
					"\nQuantities to be Compared with Log Uniform RV:\n",
					toCompareWithLogU,
					"\nlog(U):\n",
					Ulog,
					"\n"
				)	

			statesUpdatedByRandomWalk <<- 
				Ulog < toCompareWithLogU


			if ( detailedOutput ) 
				cat(
					"\nUpdated Steps:\n",
					statesUpdatedByRandomWalk,
					"\n"
				)
		},

		updateAfterRandomWalk = function()
		{
			lastStatesLogUnDensities[ statesUpdatedByRandomWalk ] <<-	
				lastProposalLogUnDensities[ statesUpdatedByRandomWalk ]

			stateSpace$updateCurrentStates(statesUpdatedByRandomWalk )
		}, 

				###### swap sphere ######

		swap = function()
			#### The swap step of the algorithm.
		{
			if ( any( statesUpdatedByRandomWalk ) ) 
			{
				transpositionsForUpdate <- findTranspositionsForUpdateAfterRandomWalk() 		

				lastSwapUProbabilities[ transpositionsForUpdate ] <<- 
					updateSwapUProbabilities( 
						swapIwithJ 				= FALSE,
						transpositionsForUpdate 	= transpositionsForUpdate
 					)	
			}

		},


		findTranspositionsForUpdateAfterRandomWalk = function()
			#### Finds numbers of transpositions in the lexical ordering whose probabilities must be updated after the random walk phase. 
		{
			return(
				ifelse( 
					all(statesUpdatedByRandomWalk),
					1:noOfTranspositions, 
					{
						setdiff(
							1:noOfTranspositions,
							translateTranspositionsToLexical(
								generateTranspositions(
									(1:noOfTemperatures)[
										!statesUpdatedByRandomWalk
									]
								)							
							)			
						)		
					}
				)
			)
		},


		generateTranspositions = function(
			lexicals
		)
			#### Creates a matrix that enlists all possible transpositions of the supplied integers. 
		{
			noOfLexicals 	<- length(lexicals)				
							
			return(		# do.call will apply cbind recursively to all enlisted objects.
				do.call(
					cbind, 
					lapply( 
						1:(noOfLexicals -1),
						function( number )
						{
							noOfCols	<- noOfLexicals-number
							return( 
								matrix( 
									c( 
										rep.int(x=lexicals[number], times=noOfCols), 
										lexicals[ (number+1):noOfLexicals] 
									), 
									ncol = noOfCols,
									nrow = 2,
									byrow= TRUE 		
								)
							)							
						}
					)
				) 
			)
		},


		translateLexicalToTranspositions = function( 
			lexicals 
		)
		{
			return( translatorFromLexicOrderToTranspositions[, lexicals ] )
		},

		translateTranspositionsToLexical = function(
			transpositions 	# Stored in a matrix.
		)
		{
			return(
				apply(
					transpositions,
					2,
					function( transposition )
					{
						i <- transposition[1]
						j <- transposition[2]

						return(
							(i-1)*(No_of_Chains-i/2) + j - i
						)
					}
				)
			)
		},


		swapStrategy = function(
			transposition
		)	
		{
			i <- transposition[1]
			j <- transposition[2] 	
			s <- strategyNumber

			tmp <- currentStatesLogDensities[i] - currentStatesLogDensities[j]

			tmp <- 
				exp(
					-ifelse( 
						s==2, 
						tmp, 
						abs(tmp) 
					)*
					ifelse( 
						s==3|s==4, 
						inverseTemperatures[i] - inverseTemperatures[j], 1)*
						ifelse( 
							s ==4,
							1/{1 + stateSpace$measureQuasiDistance(i,j) },
							1 
						)

				) 

			tmp <- ifelse( s==2 & tmp >1, 1, tmp )
			
			return( tmp )	
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