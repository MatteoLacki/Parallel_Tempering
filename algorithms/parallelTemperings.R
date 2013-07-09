
parallelTempering <- setRefClass(
	Class		= "ParallelTemperings",
	contains	= "Algorithms",

###########################################################################
								# Fields

	fields		= list(
#<fields>	
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

			## A vector of integers: overall number of accepted random-walk proposals.
		rejectedRandomWalksNo	= "integer",

			## A vector of integers: overall number of accepted transposition-swap proposals.
		acceptedSwapsNo			= "integer",

			## Triggers detailed output during the simulation.
		detailedOutput			= "logical",
#</fields>		

			## Is the swap probability independent of point in space?	
		simpleSwap				= "logical",

			## If a state-independent strategy was chosen, this vector will store the possible states. In lexicographic order.
		possibleSwaps 			= "integer"
	),

###########################################################################
								# Methods

	methods 	= list(	


		############################################################
				# Initialisation
				
#<method>
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

			insertTranspositions()
			insertStrategyNo( strategyNo )

			acceptedRandomWalksNo	<<- rep.int(0L, times = temperaturesNo)
			rejectedRandomWalksNo	<<- rep.int(0L, times = temperaturesNo)
			detailedOutput			<<- detailedOutput
		},

#<method>
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

				# Strategies 5 and 6 are state independent.
			simpleSwap 	<<- ifelse( strategyNo %in% c(5,6) , TRUE, FALSE )

				# Uniform swaps on all possible transpositions
			if ( strategyNo == 5)
			{
				possibleSwaps 	<<- 1:transpositionsNo 
			}

				# Uniform swaps on neighbouring transpositions
			if ( strategyNo == 6)
			{
				possibleSwaps 	<<- getNeighbours()
			}
		},


#<method>
		getNeighbours = function()
		{
			return( 
				sapply(
					1:(temperaturesNo-1),
					function( chainNo ) {				
						as.integer(
							(chainNo - 1)*( temperaturesNo - chainNo/2 ) + 1
						)
					}	
				)
			)
		},		


#<method>
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

#<method>
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

#<method>
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

#<method>
		parallelTemperingShow	= function()
			#### Shows the initialised fields before the simulation.
		{
			cat('\nThe Parallel Tempering inputs are here: \n')
			cat('Temperatures: ', temperatures, '\n')
			cat('Number of chains/temperatures: ', temperaturesNo, '\n')	
			cat('Chosen swap-strategy number: ', strategyNo, '\n')
			cat('Number of transpositions: ', transpositionsNo, '\n')	
			cat('State-independent swaps: ', ifelse( simpleSwap, ' yes',' no'), '\n')	

			if( simulationFinished )
			{
				cat("Transpostion history:\n")
				print( transpositionHistory )
				cat("\n")

				cat("Percentage of accepted-rejected random-walks:\n")
				acceptance <- 
					rbind( temperatures, acceptedRandomWalksNo/iterationsNo, rejectedRandomWalksNo/iterationsNo)
				acceptance <- as.data.frame( acceptance )

				row.names(acceptance) 	<- 
					c("temperatures","accepted", "rejected")
				colnames(acceptance) 	<- 1:temperaturesNo
				print( acceptance )
				cat("\n")
							
				
				cat("\n")
			}
		},

#<method>	
		show	= function()
			#### Calls the father-class show method followed by its own show method.
		{
			algorithmShow()
			parallelTemperingShow()			
		},

#<method>
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
					
#<method>
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

#<method>
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

#<method>
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

#<method>
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

			if ( !all( updatedStates ) )
			{
				rejectedRandomWalksNo[ !updatedStates ] <<- 
					rejectedRandomWalksNo[ !updatedStates ] + 1L
			}

			stateSpace$updateStatesAfterRandomWalk( anyUpdate, updatedStates )
		}, 


				###### swap sphere ######
#<method>
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

#<method>
		swapProposalGeneration = function()
			#### Generates a random swap based on the last step swap  unnormalised probabilities.
		{
			if( simpleSwap ) {
				swapProposalLexic <<-
					sample(
						possibleSwaps,  
						size = 1
					)
			} else {
				swapProposalLexic <<-
					sample(
						1:transpositionsNo,  
						size = 1,
						prob = lastSwapUProbs
					) 
			}

			swapProposal <<- 	
				translateLexicToTranspositions( swapProposalLexic )

			tmpUpdatedStates 				<- rep(FALSE, temperaturesNo)
		
				# The pair of states get the update tag here.
			tmpUpdatedStates[swapProposal] 	<- TRUE
			
			updatedStates <<- tmpUpdatedStates
		},

#<method>
		swapRejectionAndUpdate = function(
			iteration
		)
			#### Performs the rejection in the swap step and the resulting  update. 
		{
			if (!simpleSwap) {

					# The procedure for strategies 1-4.

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

			} else {

				proposalInverseTemperatures <- inverseTemperatures[ swapProposal ]

				targetLogDensities		<- lastStatesLogUDensities[ swapProposal ]

					# The procedure for state-independent strategies.

				logAlpha <- 
					(
						proposalInverseTemperatures[1] -
						proposalInverseTemperatures[2]				
					)*
					(
						targetLogDensities[2] - 
						targetLogDensities[1]
					)
			}

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
				if (!simpleSwap) {	
					lastSwapUProbs <<- proposalUProbs 
				}

				transpositionHistory[ iteration ] <<- swapProposalLexic

				if ( detailedOutput ) 
				cat(
					"\nAccepted random swap: ",
					swapProposal,
					"\n"
				)
			} else
				if ( detailedOutput ) 
				cat(
					"\nNo swap accepted.\n"
				)

			
			stateSpace$updateStatesAfterSwap( proposalAccepted, swapProposal )
		},

#<method>			
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

#<method>
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

#<method>
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

#<method>
		translateLexicToTranspositions = function( 
			lexics 
		)
			#### Translates transpositions ennumbered lexicographically into pairs of corresponding numbers. 
		{
			return( translatorFromLexicOrderToTranspositions[, lexics ] )
		},

#<method>
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

#<method>
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