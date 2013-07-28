
parallelTempering <- setRefClass(
	Class		= "ParallelTemperings",
	contains	= "MetropolisHastings",

###########################################################################
								# Fields

	fields		= list(

			## Temperature levels for the parallel tempering algorithm.
		temperatures		= "numeric",

			## 1/TemperatureLevel
		inverseTemperatures = "numeric",

			## Number of swapping states strategy.
		strategyNo 		= "integer",

			## Matrix with two rows with pairs of possible pairs in columns.
		translatorFromLexicOrderToTranspositions = "matrix",

			## Number of transpositions in the lexicographic order.
		transpositionsNo	= "integer",

			## Vector with probabilities of current pair swaps. Indexed by transpositions in lexicographic ordering.
		lastSwapUProbs		= "numeric",

			## Swap proposal in the lexicographic ordering.
		swapProposalLexic	= "integer",

			## Swap proposal as a transposition pair.
		swapProposal 		= "integer", 
	
			## An integer value describing the number of swap in the lexicographical order. -1 corresponds to swap randomWalkRejection.
		lastTranspositionNo		= "integer",

			## A vector of integers: all accepted transpositions enlisted. As long as the provided number of iterations to be executed. When rejected, we put 0. It is also filled with zeros at the beginning.
		transpositionHistory	= "factor",

			## A vector of integers: overall number of accepted transposition-swap proposals.
		acceptedSwapsNo			= "integer"		
	),

###########################################################################
								# Methods

	methods 	= list(	


		############################################################
				# Initialisation
				

		initializeParallelTempering	= function(
			temperatures 		= numeric(0),
			strategyNo			= 1L
			)
			#### Initializes the parallel-tempering-specific fields.
		{
			temperatures 		<<- temperatures
			inverseTemperatures <<- 1/temperatures

			insertStrategyNo( strategyNo )
			insertTranspositions()
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
		 		generateTranspositions( 1:chainsNo )
				
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
			iterationsNo 	= NULL,
			temperatures 	= numeric(0),
			strategyNo		= 1L,
			detailedOutput	= FALSE,
			chainsNo 		= 0L
			)
			#### Splits the initialization to general Simulations initialization and parallel-tempering-specific initialization.
		{
			if ( !is.null(iterationsNo)){
				initializeAlgorithm(
					iterationsNo 	= iterationsNo 
				)
	
				initializeMetropolisHastings( 
					chainsNo		= chainsNo,
					detailedOutput	= detailedOutput
				)
	
				initializeParallelTempering(
					temperatures 	= temperatures,
					strategyNo		= strategyNo
				)
	
				insertChainNames()
			}
		},


		insertChainNames = function() 
		{
			tmpNames <- character( chainsNo )

			for ( i in 1:chainsNo ) {
				tmpNames[i] <- paste( 
					'temp=', 
					temperatures[i], 
					sep="", collapse="" 
				)
			}

			chainNames <<- tmpNames
		},


		prepareSimulation = function()
			#### Initialises values needed before the simulation.
		{
				# Initially everything is new.
			updatedStates <<- rep( TRUE, chainsNo)

				# Current states must get at least once calculated all without any updates.
			lastStatesLogUDensities <<-  
				stateSpace$proposeLogsOfUMeasures()

			lastSwapUProbs	<<- updateSwapUProbs( translatorFromLexicOrderToTranspositions )
		},

		############################################################
				# Visualisation


		showParallelTempering = function()
			#### Shows the initialised fields before the simulation.
		{
			cat('\nThe Parallel Tempering inputs are here: \n')
			cat('Temperatures: ', temperatures, '\n')
			cat('Chosen swap-strategy number: ', strategyNo, '\n')
			cat('Number of transpositions: ', transpositionsNo, '\n')	

			if( simulationFinished )
			{
				cat("Transpostion history:\n")
				print( transpositionHistory )
				cat("\n")
			}
		},

	
		show	= function()
			#### Calls the father-class show method followed by its own show method.
		{
			showAlgorithm()
			showMetropolisHastings()
			showParallelTempering()			
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



				

		updateAfterRandomWalk = function()
		{
				# Updating state space..
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
		},


		getLogAlpha = function()
		{
			return( 
				inverseTemperatures * 
				(proposalLogUDensities - lastStatesLogUDensities) 
			)
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
			tmpUpdatedStates 				<- rep(FALSE, chainsNo)
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
								(1:chainsNo)[
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
							(i-1)*(chainsNo-i/2) + j - i
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
# 	'chainsNo',
# 	'strategyNo'
# )