ParallelTempering <- setRefClass(
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
		lastSwapUProbs		= "numeric",
	
		# 	## Vector with probabilities of current pair swaps.
		# lastProposalSwapProbabilities	= "numeric",

			## Vector with logs of unnormalised densities evaluated at current states. They are not yet multiplied by the inverse temperatures: this vector does not correspond to evaluation of the tensorised target measure.
		lastStatesLogUDensities	= "numeric",

			## Vector with logs of unnormalised densities evaluated at current proposal. They are not yet multiplied by the inverse temperatures: this vector does not correspond to evaluation of the tensorised target measure. 

			#This can be turned into a local variable.
		proposalLogUDensities	= "numeric",

			## Vector with boolean values. TRUE when a state did get updated in either Random Walk phase or Swap phase of the algorithm.
		updatedStates			= "logical",	

			## An integer value describing the number of swap in the lexicographical order. -1 corresponds to swap randomWalkRejection.
		noOfLastTransposition	= "integer",

			## A vector of integers : all accepted transpositions enlisted. As long as the provided number of iterations to be executed. When rejected, we put -1.
		transpositionHistory	= "integer",

			## Triggers detailed output during the simulation.
		detailedOutput			= "logical"
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
			if (length(temperatures) == 0)
			{
				cat(
					'I did not receive any temperatures. I shall therefore proceed with rather arbitrary choice of 5 temperature levels 1<2<3<4<5.'
				)
				tmpTemp <- 1:5	# Can add here some global constant.
			} else 
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
		
			temperatures 		<<- tmpTemp
			inverseTemperatures <<- 1/tmpTemp
			noOfTemperatures 	<<- length(tmpTemp)

						
			tmpStrategyNumber	<- as.integer(strategyNumber)

			if ( is.na(tmpStrategyNumber) || tmpStrategyNumber < 0 )
			{		 
				stop(
					"Inappropriate stregy number. Please enter an integer value."
				)
			} else
			{	
				strategyNumber	<<- tmpStrategyNumber
			}
			


			translatorFromLexicOrderToTranspositions <<- 
		 		generateTranspositions( 
		 			1:noOfTemperatures 
		 		)
				

			#noOfTranspositions <<- noOfTemperatures*(noOfTemperatures-1)/2
			noOfTranspositions 	<<- 
				ncol( translatorFromLexicOrderToTranspositions )

			stateSpace	<<- 
				realStateSpace$new(
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
			updatedStates <<- rep( TRUE, noOfTemperatures)

				# Current states must get at least once calculated all without any updates.
			lastStatesLogUDensities <<-  
				stateSpace$getProposalLogsOfUnnormalisedDensities()

			noOfLastTransposition 	<<- -1L
			transpositionHistory	<<-	integer(noOfIterations)

			detailedOutput			<<- detailedOutput

			lastSwapUProbs			<<- updateSwapUProbs( translatorFromLexicOrderToTranspositions )
				
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
			cat('Number of chains/temperatures: ', noOfTemperatures, '\n')	
			cat('Chosen swap-strategy number: ', strategyNumber, '\n')
			cat('Number of transpositions: ', noOfTranspositions, '\n')	
			cat('Logs of unnormalised densities in last states:\n', lastStatesLogUDensities, '\n')
			cat('Initial need for update: ', updatedStates, '\n')
			
			cat('Initial swap U probabilities:\n', lastSwapUProbs, '\n')

							

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

			swap( iteration )	

		},


				###### random walk sphere ######

		randomWalk = function()
		{
			proposalLogUDensities <<- 		
				stateSpace$randomWalkProposal()

			if ( detailedOutput ) 
				cat(
					"\nPrevious Log Densities:\n",
					lastStatesLogUDensities,
					"\nProposal Log Densities:\n",
					proposalLogUDensities,
					"\n"
				)
			
			randomWalkRejection()
			
			updateAfterRandomWalk() # both logdensities and current states.
		},


		randomWalkRejection = function()
		{
			Ulog <- log( runif( noOfTemperatures ) )

			logAlpha <- 
				sapply(
					1:noOfTemperatures,
					function( k )
					{
						inverseTemperatures[k]*
						(
							proposalLogUDensities[k] -
							lastStatesLogUDensities[k]
						)

					}
				)

			if ( detailedOutput ) 
				cat(
					"\nQuantities to be Compared with Log Uniform RV:\n",
					logAlpha,
					"\nlog(U):\n",
					Ulog,
					"\n"
				)	

			updatedStates <<- 
				Ulog < logAlpha


			if ( detailedOutput ) 
				cat(
					"\nUpdated Steps:\n",
					updatedStates,
					"\n"
				)
		},

		updateAfterRandomWalk = function()
		{
			anyUpdate <- any( updatedStates )

			if ( anyUpdate ) 
			{				# Updating state space..
				lastStatesLogUDensities[ updatedStates ] <<-	
					proposalLogUDensities[ updatedStates ]

# updating unnormalised probabilities of swaps caused by random walk changes.
				transpositionsForUpdate <- findTranspositionsForUpdate() 

				lastSwapUProbs[ transpositionsForUpdate ] <<- 
					updateSwapUProbs( 
						translateLexicToTranspositions(
							transpositionsForUpdate
						) 
					)	
			}
			stateSpace$updateStatesAfterRandomWalk( anyUpdate, updatedStates )
		}, 

				###### swap sphere ######


		swap = function(
			iteration
		)
			#### The swap step of the algorithm.
		{			
			proposalLexic <-
				sample(
						1:noOfTranspositions,  
						size = 1,
						prob = lastSwapUProbs
				) 

			proposal <- 
				translateLexicToTranspositions( proposalLexic	)
			
				# Here rather than updating states we update information about states.
			tmpUpdatedStates 			<- rep(FALSE, noOfTemperatures)
			tmpUpdatedStates[proposal] 	<- TRUE
			
			updatedStates <<- tmpUpdatedStates
			rm( tmpUpdatedStates )

			transpositionsUpdateForProposal <- 
				findTranspositionsForUpdate()	
					
			proposalUProbs 	<- lastSwapUProbs

			proposalUProbs[ transpositionsUpdateForProposal ] <- 
				updateSwapUProbs( 
					translateLexicToTranspositions(
						transpositionsUpdateForProposal
					) 
				)
			
			proposalLogProb	<- 
				log( proposalUProbs[ proposalLexic ] ) - 
				log( sum( proposalUProbs ) )

			lastLogProb		<- 
				log( lastSwapUProbs[ proposalLexic ] ) - 
				log( sum( lastSwapUProbs ) )

			proposalInverseTemperatures <- inverseTemperatures[ proposal ]

			targetLogDensities		<- lastStatesLogUDensities[ proposal ]

			logAlpha <- 
				(
					proposalInverseTemperatures[1] -
					proposalInverseTemperatures[2]				
				)*
				(
					targetLogDensities[2] - 
					targetLogDensities[1]
				)+
				proposalLogProb -
				lastLogProb

			Ulog <- log( runif(1) )
				
			if ( detailedOutput ) 
			cat(
				"\nQuantities to be Compared with Log Uniform RV:\n",
				logAlpha,
				"\nlog(U):\n",
				Ulog,
				"\n"
			)
			
			proposalAccepted <- Ulog < logAlpha	

			if ( proposalAccepted )
			{
				lastSwapUProbs <<- proposalUProbs 

				transpositionHistory[ iteration ] <<- proposalLexic
			} else
			{
				transpositionHistory[ iteration ] <<- -1L
			}		
			
			stateSpace$updateStatesAfterSwap( proposalAccepted, proposal )

			if ( detailedOutput ) 
			{	
				cat( "\nStates after Random Swap:\n" ) 
				print( stateSpace$lastStates )
				cat( "\n\n" )	
			}
		},

			# =TO=DO= : rationalize after speed tests: maybe can vectorize swapStrategy.
		updateSwapUProbs = function(
			transpositionsForUpdate
		)
		{
			return(
				apply(
					transpositionsForUpdate,
					2,
					function( transposition ) swapStrategy( transposition )	  
				)
			)
		},


		findTranspositionsForUpdate = function()
			#### Finds numbers of transpositions in the lexical ordering whose probabilities must be updated after the random walk phase. 
		{
				# Changes on all chains or on all chains but one require complete reevaluation of everything. Hence the criterion.
			return(
				if( 
					ifelse( 
						sum(
							ifelse( updatedStates, 0, 1 )
						) %in% 0:1,
						TRUE,
						FALSE
					)	
				)
				{
					1:noOfTranspositions
				} else  
				{
					setdiff(
						1:noOfTranspositions,
						translateTranspositionsToLexic(
							generateTranspositions(
								(1:noOfTemperatures)[
									!updatedStates
								]
							)							
						)			
					)		
				}
			)
		},


		generateTranspositions = function(
			chainNumbers
		)
			#### Creates a matrix that enlists all possible transpositions of supplied indices of the temperature vector. 
		{
			noOfChainNumbers 	<- length(chainNumbers)				
							
			return(		
				do.call(
					cbind, 
					lapply( 
						1:(noOfChainNumbers -1),
						function( number )
						{
							noOfCols	<- noOfChainNumbers-number
							return( 
								matrix( 
									c( 
										rep.int(x=chainNumbers[number], times=noOfCols), 
										chainNumbers[ (number+1):noOfChainNumbers] 
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


		translateLexicToTranspositions = function( 
			lexics 
		)
		{
			return( translatorFromLexicOrderToTranspositions[, lexics ] )
		},

		translateTranspositionsToLexic = function(
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
							(i-1)*(noOfTemperatures-i/2) + j - i
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

			tmp <- lastStatesLogUDensities[i] - lastStatesLogUDensities[j]

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
ParallelTempering$lock( 
	'temperatures',
	'inverseTemperatures',
	'noOfTemperatures',
	'strategyNumber'
)
