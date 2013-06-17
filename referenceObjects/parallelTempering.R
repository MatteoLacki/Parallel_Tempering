parallelTempering <- setRefClass(
	Class		= "ParallelTemperings",
	contains	= "Algorithms",

###########################################################################
								# Fields

	fields		= list(
	
			## Temperature levels for the parallel tempering algorithm.
		temperatures		= "numeric",

			## 1/TemperatureLevel
		inverseTemperatures = "numeric",

			## Number of swapping states strategy.
		strategyNumber 		= "integer",

			## Number of temperature levels.						
		temperaturesNo	= "integer",	

			## Matrix with two rows with pairs of possible pairs in columns.
		translatorFromLexicOrderToTranspositions = "matrix",

			## Number of transpositions in the lexicographic order.
		transpositionsNo	= "integer",

			## Vector with probabilities of current pair swaps.
		lastSwapUProbs		= "numeric",

			## Swap proposal in the lexicographic ordering.
		swapProposalLexic	= "integer",

			## Swap proposal as a transposition pair.
		swapProposal 		= "integer", 
	
			## Vector with logs of unnormalised densities evaluated at current states. They are not yet multiplied by the inverse temperatures: this vector does not correspond to evaluation of the tensorised target measure.
		lastStatesLogUDensities	= "numeric",

			## Vector with logs of unnormalised densities evaluated at current proposal. They are not yet multiplied by the inverse temperatures: this vector does not correspond to evaluation of the tensorised target measure. 

			#This can be turned into a local variable.
		proposalLogUDensities	= "numeric",

			## Vector with boolean values. TRUE when a state did get updated in either Random Walk phase or Swap phase of the algorithm.
		updatedStates			= "logical",	

			## An integer value describing the number of swap in the lexicographical order. -1 corresponds to swap randomWalkRejection.
		lastTranspositionNo	= "integer",

			## A vector of integers : all accepted transpositions enlisted. As long as the provided number of iterations to be executed. When rejected, we put 0. It is also filled with zeros at the beginning.
		transpositionHistory	= "integer",

			## Triggers detailed output during the simulation.
		detailedOutput			= "logical"
	),

###########################################################################
								# Methods

	methods 	= list(	


		############################################################
				# Initialisation
				

		initializeParallelTempering	= function(
			temperatures 		= numeric(0),
			strategyNumber		= 1L,
			detailedOutput		= FALSE
			)
			#### Initializes the parallel-tempering-specific fields.
		{
			temperatures 		<<- temperatures
			inverseTemperatures <<- 1/tmpTemp
			temperaturesNo 		<<- length(tmpTemp)

			insertStrategyNo( strategyNumber )
			insertTranspositions()

			lastTranspositionNo 	<<- -1L
			transpositionHistory	<<-	integer( iterationsNo )
			detailedOutput			<<- detailedOutput
		},


		insertStrategyNo = function(
			strategyNumber
		)
		{
			tmpStrategyNumber	<- as.integer(strategyNumber)

			if ( is.na(tmpStrategyNumber) || tmpStrategyNumber < 0 )
			{		 
				stop(
					"Inappropriate stregy number. Right now you can choose among strategies from 1 to 4."
				)
			} else
			{	
				strategyNumber	<<- tmpStrategyNumber
			}
		},


		insertTranspositions = function()
		{
			translatorFromLexicOrderToTranspositions <<- 
		 		generateTranspositions( 1:temperaturesNo )
				
			transpositionsNo 	<<- 
				ncol( translatorFromLexicOrderToTranspositions )			
		},


		initialize 				= function(
			iterationsNo 		= 0L,
			temperatures 		= numeric(0),
			strategyNumber		= 1L,
			detailedOutput		= FALSE
			)
			#### Splits the initialization to general Simulations initialization and parallel-tempering-specific initialization.
		{
			algorithmInitializator(
				iterationsNo 		= iterationsNo 
			)

			initializeParallelTempering( 
				temperatures 		= temperatures,
				strategyNumber		= strategyNumber,
				detailedOutput		= detailedOutput
			)
		},


		prepareSimulation = function()
			#### Initialises values needed before the simulation.
		{
				# Initially everything is new.
			updatedStates 			<<- rep( TRUE, temperaturesNo)

				# Current states must get at least once calculated all without any updates.
			lastStatesLogUDensities <<-  
				stateSpace$proposeLogsOfUMeasures()

			lastSwapUProbs			<<- updateSwapUProbs( translatorFromLexicOrderToTranspositions )		
		},

		############################################################
				# Visualisation


		parallelTemperingShow	= function()
			#### Shows the initialised fields before the simulation.
		{
			cat('\nThe Parallel Tempering inputs are here: \n')
			cat('Temperatures: ', temperatures, '\n')
			cat('Number of chains/temperatures: ', temperaturesNo, '\n')	
			cat('Chosen swap-strategy number: ', strategyNumber, '\n')
			cat('Number of transpositions: ', transpositionsNo, '\n')	
			cat('Logs of unnormalised densities in last states:\n', lastStatesLogUDensities, '\n')
			cat('Initial need for update: ', updatedStates, '\n')
			cat('Initial swap U probabilities:\n', lastSwapUProbs, '\n')
			print( stateSpace$showState() )
		},

	
		show	= function()
			#### Calls the father-class show method followed by its own show method.
		{
			algorithmShow()
			parallelTemperingShow()			
		},
	

		############################################################
				# Algorithmic Methods
					

		makeStepOfTheAlgorithm	= function( 
			iteration 
		)
			#### Makes one step of random walk followed by one swap step.
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
			#### Performs the random walk step: it asks the state-space to generate the logs of unnormalised probabilities evaluated in the proposed points and then performs the usual rejection part. All this could be done parallely if it was needed - this feature will be shipped with version 2.0.
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
			#### Here the Hastings quotients get compared with randomly generated values from the unit interval. All values are taken in logs for numerical stability.
		{
			Ulog <- log( runif( temperaturesNo ) )

			logAlpha <- 
				sapply(
					1:temperaturesNo,
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
			#### This procedure updates the state space after an operation consisting of accepting any new proposal in the random-walk phase of the algorithm. Updates are also needed in the probabilities of the last states stored in a field in the parallel-tempering object.
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
			#### This procedure performs the swap phase of an iteration of the algorithm.
		{			
			swapProposalGeneration()
			swapRejectionAndUpdate( iteration )

			if ( detailedOutput ) 
				{	
					cat( "\nStates after Random Swap:\n" ) 
					print( stateSpace$lastStates )
					cat( "\n\n" )	
				}
		},


		swapProposalGeneration = function()
			#### Generates a random swap based on the last step swap  unnormalised probabilities.
		{
			swapProposalLexic <<-
				sample(
						1:transpositionsNo,  
						size = 1,
						prob = lastSwapUProbs
				) 

			swapProposal <<- 
				translateLexicToTranspositions( swapProposalLexic )
		},


		swapRejectionAndUpdate = function(
			iteration
		)
			#### Performs the rejection in the swap step and the resulting  update. 
		{
			tmpUpdatedStates 			<- rep(FALSE, temperaturesNo)
			tmpUpdatedStates[swapProposal] 	<- TRUE
			
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
				log( proposalUProbs[ swapProposalLexic ] ) - 
				log( sum( proposalUProbs ) )

			lastLogProb		<- 
				log( lastSwapUProbs[ swapProposalLexic ] ) - 
				log( sum( lastSwapUProbs ) )

			proposalInverseTemperatures <- inverseTemperatures[ swapProposal ]

			targetLogDensities		<- lastStatesLogUDensities[ swapProposal ]

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
				transpositionHistory[ iteration ] <<- swapProposalLexic
			} 		
			
			stateSpace$updateStatesAfterSwap( proposalAccepted, swapProposal )
		},

			# =TO=DO= : rationalize after speed tests: maybe can vectorize swapStrategy.
		updateSwapUProbs = function(
			transpositionsForUpdate
		)
			#### Update values of unnormalised probabilities of swap.
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
					1:transpositionsNo
				} else  
				{
					setdiff(
						1:transpositionsNo,
						translateTranspositionsToLexic(
							generateTranspositions(
								(1:temperaturesNo)[
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
			chainNumbersNo 	<- length(chainNumbers)				
							
			return(		
				do.call(
					cbind, 
					lapply( 
						1:(chainNumbersNo -1),
						function( number )
						{
							colsNo	<- chainNumbersNo-number
							return( 
								matrix( 
									c( 
										rep.int(x=chainNumbers[number], times=colsNo), 
										chainNumbers[ (number+1):chainNumbersNo] 
									), 
									ncol = colsNo,
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
			#### Translates transpositions ennumbered lexicographically into pairs of corresponding numbers. 
		{
			return( translatorFromLexicOrderToTranspositions[, lexics ] )
		},


		translateTranspositionsToLexic = function(
			transpositions 	
		)
			#### Lexicographically orderes given pairs of transpositions.
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
							(i-1)*(temperaturesNo-i/2) + j - i
						)
					}
				)
			)
		},


		swapStrategy = function(
			transposition
		)	
			#### A function that calculates the unnormalised probabilities of swaps, based on the preinserted strategy number.
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
parallelTempering$lock( 
	'temperatures',
	'inverseTemperatures',
	'temperaturesNo',
	'strategyNumber'
)
