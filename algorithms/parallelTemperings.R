
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
		strategyNo 		= "integer",

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
		lastTranspositionNo		= "integer",

			## A vector of integers: all accepted transpositions enlisted. As long as the provided number of iterations to be executed. When rejected, we put 0. It is also filled with zeros at the beginning.
		transpositionHistory	= "factor",

			## A vector of integers: overall number of accepted random-walk proposals.
		acceptedRandomWalksNo	= "integer",

			## A vector of integers: overall number of accepted transposition-swap proposals.
		acceptedSwapsNo			= "integer",

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
			strategyNo			= 1L,
			detailedOutput		= FALSE
			)
			#### Initializes the parallel-tempering-specific fields.
		{
			temperatures 		<<- temperatures
			inverseTemperatures <<- 1/temperatures
			temperaturesNo 		<<- length(temperatures)

			insertStrategyNo( strategyNo )
			insertTranspositions()

			acceptedRandomWalksNo	<<- rep.int(0L, times = temperaturesNo)
			detailedOutput			<<- detailedOutput
		},


		insertStrategyNo = function(
			strategyNo
		)
		{
			tmpStrategyNo	<- as.integer( strategyNo )

			if ( is.na( tmpStrategyNo ) || tmpStrategyNo < 0 )
			{		 
				stop(
					"Inappropriate stregy number. Right now you can choose among strategies from 1 to 4."
				)
			} else
			{	
				strategyNo	<<- tmpStrategyNo
			}
		},


		insertTranspositions = function()
		{
			translatorFromLexicOrderToTranspositions <<- 
		 		generateTranspositions( 1:temperaturesNo )
				
			transpositionsNo 	<<- 
				ncol( translatorFromLexicOrderToTranspositions )			

			lastTranspositionNo <<- -1L

			tmpTranspositionHistory <- as.factor( integer( iterationsNo ) )
			tmpTranspositionHistory <- 
				ordered(
					tmpTranspositionHistory,
					levels = 0:transpositionsNo
				)

			transpositionHistory<<-	tmpTranspositionHistory
		},


		initialize = function(
			iterationsNo 	= 0L,
			temperatures 	= numeric(0),
			strategyNo		= 1L,
			detailedOutput	= FALSE
			)
			#### Splits the initialization to general Simulations initialization and parallel-tempering-specific initialization.
		{
			initializeAlgorithm(
				iterationsNo = iterationsNo 
			)

			initializeParallelTempering( 
				temperatures 	= temperatures,
				strategyNo		= strategyNo,
				detailedOutput	= detailedOutput
			)
		},


		prepareSimulation = function()
			#### Initialises values needed before the simulation.
		{
				# Initially everything is new.
			updatedStates <<- rep( TRUE, temperaturesNo)

				# Current states must get at least once calculated all without any updates.
			lastStatesLogUDensities <<-  
				stateSpace$proposeLogsOfUMeasures()

			lastSwapUProbs	<<- updateSwapUProbs( translatorFromLexicOrderToTranspositions )
		},

		############################################################
				# Visualisation


		parallelTemperingShow	= function()
			#### Shows the initialised fields before the simulation.
		{
			cat('\nThe Parallel Tempering inputs are here: \n')
			cat('Temperatures: ', temperatures, '\n')
			cat('Number of chains/temperatures: ', temperaturesNo, '\n')	
			cat('Chosen swap-strategy number: ', strategyNo, '\n')
			cat('Number of transpositions: ', transpositionsNo, '\n')	

			if( simulationFinished )
			{
				cat("Transpostion history:\n")
				print( transpositionHistory )
				cat("\n")

				cat("Percentage of accepted random-walks:\n")
				print( acceptedRandomWalksNo/iterationsNo )
				cat("\n")
				
			}
		},

	
		show	= function()
			#### Calls the father-class show method followed by its own show method.
		{
			algorithmShow()
			parallelTemperingShow()			
		},


		plotHistory = function()
		{
			xAxisTags <- 
				apply(
					translatorFromLexicOrderToTranspositions,
					2,
					function( transposition )
					{
						paste(
							"(", 
							transposition[1], 
							",",
							transposition[2],
							")", 
							sep="", 
							collapse=""
						)	
					}
				)

			xAxisTags <- c("No swap", xAxisTags)	

			p <-qplot( 
					transpositionHistory, 
					geom="bar"
				) +
				scale_x_discrete(
					breaks = 0:transpositionsNo,
					labels = xAxisTags
				) + 
				theme(
					axis.title.x=element_text(colour="darkred", size=14),
					axis.title.y=element_text(colour="darkred", size=14),
					plot.title=element_text(size=rel(1.5), colour="darkred")
				) + 
				labs(
					x='Transposition',
					y='Counts',
					title ="Swaps distribution"
				)


			return( p )	
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
				lastStatesLogUDensities[ updatedStates ]<<-	
					proposalLogUDensities[ updatedStates ]

				acceptedRandomWalksNo[ updatedStates ] 	<<-
					acceptedRandomWalksNo[ updatedStates ] + 1L 

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
			s <- strategyNo

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
# parallelTempering$lock( 
# 	'temperatures',
# 	'inverseTemperatures',
# 	'temperaturesNo',
# 	'strategyNo'
# )