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
		strategyNo 			= "integer",

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

			## A vector numbered from 1 to transpositionNo where counts of swaps and their acceptances are made.
		transpositionsHistory 	= "matrix",

			## Is the swap probability independent of point in space?	
		simpleSwap			= "logical",

			## If a state-independent strategy was chosen, this vector will store the possible states. In lexicographic order.
		possibleSwaps 		= "integer"
	),

###########################################################################
								# Methods

	methods 	= list(	


		############################################################
				# Initialisation
				
		insertStrategyNo = function(
			strategyNo
		)
		{
			tmpStrategyNo	<- as.integer( strategyNo )

			if ( is.na( tmpStrategyNo ) || tmpStrategyNo < 0 ){		 
				stop(
					"Inappropriate stregy number. Right now you can choose among strategies from 1 to 6."
				)

			} else {	
				strategyNo	<<- tmpStrategyNo
			}

				# Strategies 5 and 6 are state independent.
			simpleSwap 	<<- ifelse( strategyNo %in% c(5,6) , TRUE, FALSE )

				# Uniform swaps on all possible transpositions
			if ( strategyNo == 5){
				possibleSwaps 	<<- 1:transpositionsNo 
			}

				# Uniform swaps on neighbouring transpositions
			if ( strategyNo == 6){
				possibleSwaps 	<<- getNeighbours()
			}
		},


		getNeighbours = function()
		{
			return( 
				sapply(
					1:(chainsNo-1),
					function( chainNo ) {				
						as.integer(
							(chainNo - 1)*( chainsNo - chainNo/2 ) + 1
						)
					}	
				)
			)
		},		


		insertTranspositions = function()
		{
			translatorFromLexicOrderToTranspositions <<- 
		 		generateTranspositions( 1:chainsNo )
				
			transpositionsNo 	<<- 
				ncol( translatorFromLexicOrderToTranspositions )			

			transpositionsHistory<<- matrix(0L,ncol=transpositionsNo, nrow=2 )
		},

		
		initialize = function(
			iterationsNo 	= NULL,
			burnIn 			= 2000L,
			temperatures 	= numeric(0),
			strategyNo		= 1L,
			detailedOutput	= FALSE,
			chainsNo 		= 0L,
			integratedFunction  = function(){},
			...
		){
			if ( !is.null(iterationsNo)){

				temperatures 		<<- temperatures
				inverseTemperatures <<- 1/temperatures

				callSuper( 
					iterationsNo 		= iterationsNo,
					burnIn 				= burnIn,
					chainsNo 			= chainsNo,
					detailedOutput 		= detailedOutput,
					integratedFunction  = integratedFunction,
					...
				)

				acceptedRandomWalksNo	<<- rep.int(0L, times = chainsNo)
				
				insertTranspositions()
				insertStrategyNo( strategyNo )
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
	
		anteSimulationShow = function(...)
		{
			cat('\nUsing Parallel Tempering Algorithm\n')
			cat('\tNumber of steps: ', iterationsNo, '\n')		
			cat('\tTemperatures: ', temperatures, '\n')
			cat('\tChosen swap-strategy number: ', strategyNo, '\n')
			cat('\tNumber of transpositions: ', transpositionsNo, '\n')	
			cat('You use a swapping strategy that is ', 
				ifelse( simpleSwap, 'state-independent','state-dependent'), 
				'\n'
			)	
		},


		postSimulationShow = function(...)
		{
			if( simulationFinished )
			{
				cat("\n\n\tPercentage of accepted-rejected random-walks:\n")
				acceptance <- rbind( 
					chainNames, 
					round(acceptedRandomWalksNo/iterationsNo, digits=3),
					round(1 - acceptedRandomWalksNo/iterationsNo, digits=3)
				)

				acceptance <- as.data.frame( acceptance )

				row.names(acceptance) 	<- 
					c("temperatures","accepted", "rejected")
				colnames(acceptance) 	<- 1:chainsNo
				print( acceptance )
				cat("\n")
			}		
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

			cnt  <-	ncol( transpositionsHistory )
			data <- numeric(2*cnt)
			data[1:cnt] <- transpositionsHistory[2,]
			data[(cnt+1):(2*cnt)] <-
				transpositionsHistory[1,]-transpositionsHistory[2,]	

			data <- as.data.frame(data)
			names(data) <- 'Count'

			data$Acceptance[1:cnt] <- 'Accepted'	
			data$Acceptance[(cnt+1):(2*cnt)] <- 'Rejected'

			data$Transposition[1:cnt] <- xAxisTags	
			data$Transposition[(cnt+1):(2*cnt)] <- xAxisTags

			data$labelY[1:cnt] <- transpositionsHistory[2,]	
			data$labelY[(cnt+1):(2*cnt)] <- transpositionsHistory[1,]+max(transpositionsHistory[1,])/40


			data$relativeValue[1:cnt] <- round(
				transpositionsHistory[2,]/transpositionsHistory[1,],
				digits = 2	
			)

			data$relativeValue[(cnt+1):(2*cnt)] <- round(
				1 - data$relativeValue[1:cnt],
				digits=2
			)

			p <- ggplot( 
					data, 
					aes(x=Transposition, y=Count, fill=Acceptance)
				)+
				geom_bar(stat="identity")+
				geom_text(
					aes(y=labelY, label=relativeValue), 
					vjust=1.5, colour="black", size=4
				)+
				scale_fill_brewer(palette="Pastel1")+
				labs(
					title ="Swaps distribution"
				)

			return( p )	
		},	

		writeInfo = function(
	 		directoryToWrite,
	 		...
	 	)	
	 	{
	 		callSuper(
	 			directoryToWrite,
	 			...
	 		)

	 		write.csv2(
				list(
	 				temperatures = temperatures 			
	 			),
				file = paste(
					directoryToWrite,
					"/temperatures.csv",
					sep="",
					collapse=""
				),
				row.names 	= FALSE
			)
	 	},


		writeSwaps = function( 
			directoryToWrite,
			...
		){
	 		write.csv2(
				swapHistory(),
				file = paste(
					directoryToWrite,
					"/swapsRejections.csv",
					sep="",
					collapse=""
				),
				row.names 	= FALSE
			)
		},


		swapHistory = function(){
			return( c(transpositionsHistory[2,],transpositionsHistory[1,]) )
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

			if ( notBurning ) stateSpace$calculateBetweenSteps( iteration )
		},
				###### random walk sphere ######


		updateAfterRandomWalk = function(){
				# Updating state space..
			lastStatesLogUDensities[ updatedStates ]<<-	
				proposalLogUDensities[ updatedStates ]

			if ( notBurning ){	
				acceptedRandomWalksNo[ updatedStates ] 	<<-
					acceptedRandomWalksNo[ updatedStates ] + 1L 
			}
					
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

			if ( notBurning ){
				transpositionsHistory[1,swapProposalLexic] <<- transpositionsHistory[1,swapProposalLexic]+1L 	
			}	

			swapProposal <<- 	
				translateLexicToTranspositions( swapProposalLexic )

			tmpUpdatedStates 				<- rep(FALSE, chainsNo)
		
				# The pair of states get the update tag here.
			tmpUpdatedStates[swapProposal] 	<- TRUE
			
			updatedStates <<- tmpUpdatedStates
		},


		swapRejectionAndUpdate = function(
			iteration
		)
			#### Performs the rejection in the swap step and the resulting  update. 
		{
			if (!simpleSwap) {

			# tmpUpdatedStates 				<- rep(FALSE, chainsNo)
			# tmpUpdatedStates[swapProposal] 	<- TRUE
			
			# updatedStates <<- tmpUpdatedStates
			# rm( tmpUpdatedStates )

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

				if ( notBurning ){
					transpositionsHistory[2,swapProposalLexic] <<- transpositionsHistory[2,swapProposalLexic]+1L
				}

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

			if( s==7 ){
				tmp <- exp( -(inverseTemperatures[i] - inverseTemperatures[j])*lastStatesLogUDensities[i]  
				)
			}else{	
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
				
			}
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
